{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}


module AppHandlers.DiagTree
    ( diagInfo
    , diagHistory
    , retryQuestion

    , MoveOrCopyDiagSlide (..)
    , moveOrCopyDiagSlide

    , GetTreeException (..)
    )

where

import           GHC.Generics

import           Control.Exception                   (Exception)
import           Control.Monad                       (forM, when)
import           Control.Monad.Catch                 (MonadThrow, throwM)
import           Control.Monad.Except                (runExceptT, throwError)
import           Control.Monad.IO.Class              (MonadIO)
import           Control.Monad.Reader                (ReaderT)
import           Control.Monad.Trans.Class           (lift)
import           Data.Aeson                          as A
import qualified Data.HashMap.Strict                 as HM
import           Data.List                           (find)
import           Data.Maybe                          (isJust, isNothing)
import           Data.Tree
import           Data.Typeable                       (Typeable)
import           Data.Vector                         ((!), (//))
import qualified Data.Vector                         as Vector
import           Database.Persist
import           Database.Persist.Sql                (Single (..), fromSqlKey,
                                                      rawSql, transactionSave)
import           Database.Persist.Sql.Types.Internal
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Int

import           Snap
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.PostgresqlSimple
import           Snaplet.Auth.PGUsers                (currentUserMetaId)

import           AppHandlers.Util
import           Application
import           Carma.Model.DiagSlide.Persistent
import           Utils.HttpErrors                    (finishWithError)


-- | Check if idag is possible or has started already
diagInfo :: AppHandler ()
diagInfo = do
  caseId <- getParam "caseId"
  [Only res] <- query [sql|
    with slides as
        (select
            h.ctime, s.isRoot,
            (s.answers->h.answerIx->>'isFinal')::bool as isFinal
          from "DiagHistory" h join "DiagSlide" s on (h.slideId = s.id)
          where caseId = ?
        )
      select row_to_json(x) from
        (select
          (select diagTree
            from "SubProgram" s join casetbl c on (s.id = c.subprogram)
            where c.id = ?) as root,
          (select ctime from slides where isRoot limit 1) as started,
          (select ctime from slides where isFinal limit 1) as ended
        ) x
    |] [caseId, caseId]
  writeJSON (res :: A.Value)


-- | Join history with slides
diagHistory :: AppHandler ()
diagHistory = do
  caseId <- getParam "caseId"
  hist <- query [sql|
    select row_to_json(x) from
      (select
          h.id, to_char(h.answerTime, 'YYYY-MM-DD HH24-MI') as "answerTime",
          u.login as "answeredBy", h.answerIx as "answerIx",
          h.deprecatedBy as "deprecatedBy",
          s.header, s.body, s.answers, s.resources, s.actions
        from "DiagHistory" h
          join "DiagSlide" s on (h.slideId = s.id)
          left outer join usermetatbl u on (h.answeredBy = u.id)
        where h.caseId = ?
        order by h.ctime asc) x
    |] [caseId]
  writeJSON (map fromOnly hist :: [A.Value])


retryQuestion :: AppHandler ()
retryQuestion = do
  histId <- getParam "histId"
  Just userId <- currentUserMetaId
  _ <- execute [sql|
      with newQ(id) as
        (insert into "DiagHistory" (caseId, slideId, createdBy)
          select caseId, slideId, ?::int
            from "DiagHistory"
            where id = ?
              and deprecatedBy is null
          returning "DiagHistory".id)
        update "DiagHistory" h
          set deprecatedBy = newQ.id
          from "DiagHistory" h1, newQ
          where h1.id = ?
            and h.caseId = h1.caseId
            and h.id <> newQ.id
            and h.ctime >= h1.ctime
            and h.deprecatedBy is null
    |] (userId, histId, histId)
  writeJSON ()


data MoveOrCopyDiagSlide = MoveDiagSlide | CopyDiagSlide


data CopyMoveOperation = CopyMoveOperation
  { source      :: [Int]
  , destination :: [Int]
  } deriving (Show, Generic, FromJSON)


type DiagSlideTree = Tree (Int64, DiagSlide)


numberFromInt :: Int64 -> Value
numberFromInt = A.Number . fromIntegral


-- | Get @DiagSlide@ by a key
getDiagSlide
  :: ( BaseBackend backend ~ Database.Persist.Sql.Types.Internal.SqlBackend
     , PersistQueryRead backend
     , MonadIO m
     )
  => Int
  -> ReaderT backend m (Maybe DiagSlide)

getDiagSlide key = do
  (res :: Maybe (Entity DiagSlide)) <-
    selectFirst [ DiagSlideId ==. mkKey key ] []

  case res of
    Just entityDiagSlide -> pure $ Just $ entityVal entityDiagSlide
    Nothing              -> pure Nothing


newtype GetTreeException = InvalidId Int deriving (Show, Typeable)
instance Exception GetTreeException


-- | Get tree of @DiagSlide@s by a key
getTree
  :: ( BaseBackend backend ~ Database.Persist.Sql.Types.Internal.SqlBackend
     , PersistQueryRead backend
     , MonadThrow m
     , MonadIO m
     )
  => Int
  -> ReaderT backend m DiagSlideTree

getTree key = do
  res <- getDiagSlide key
  case res of
    Just diagSlide -> do
      let (A.Array answers) = diagSlideAnswers diagSlide
      children <- forM (Vector.toList answers) $ \(A.Object answer) -> do
        -- TODO FIXME fix for Nothing in HM.lookup
        let (Just (A.Number n)) = HM.lookup "nextSlide" answer
        getTree $ floor n
      pure $ Node (fromIntegral key, diagSlide) children
    Nothing -> throwM $ InvalidId key


-- | Walk throw DiagSlide tree and insert slides into DB.
-- | Returns - (old slide key, new slide key).
treeToCopyTransaction
  :: ( BaseBackend backend ~ Database.Persist.Sql.Types.Internal.SqlBackend
     , PersistQueryRead backend
     , PersistStoreWrite backend
     , MonadIO m
     )
  => DiagSlideTree
  -> ReaderT backend m (Int64, Int64)

treeToCopyTransaction =
  \case
    Node (oldKey, slide) [] -> do
      newKey <- insert slide
      pure (oldKey, fromSqlKey newKey)
    Node (oldKey, slide) children -> do
      childrenIds <- mapM treeToCopyTransaction children
      let (A.Array answers) = diagSlideAnswers slide

      let newAnswers = flip map (Vector.toList answers) $ \(A.Object answer) ->
                       HM.fromList $ flip map (HM.toList answer) $ \kv ->
                         case kv of
                           ("nextSlide", A.Number n) ->
                             let nextSlide = floor n :: Int64
                             in case find ((== nextSlide) . fst) childrenIds of
                                  Just (_, newChildKey) ->
                                    ("nextSlide", numberFromInt newChildKey)
                                  Nothing -> kv
                           _ -> kv

      let slide' = slide { diagSlideAnswers = A.toJSON newAnswers
                         , diagSlideIsRoot  = False
                         }

      newKey <- insert slide'
      pure (oldKey, fromSqlKey newKey)


getPasteParams :: Handler App App ([Int], [Int])
getPasteParams = do
  body <- getJSONBody :: AppHandler CopyMoveOperation
  pure (source body, destination body)


moveOrCopyDiagSlide :: MoveOrCopyDiagSlide -> AppHandler ()
moveOrCopyDiagSlide CopyDiagSlide = do
  (sourcePath, destinationPath) <- getPasteParams

  res <-
    runExceptT $ do
      when (null sourcePath) $ throwError (400, "Source is empty")
      when (null destinationPath) $ throwError (400, "Destination is empty")

      when (sourcePath == destinationPath) $
        throwError (400, "Unable to copy&paste to the same point")

      lift (with db2 $ runPersist $ getDiagSlide $ last destinationPath)
        >>= \case Just destinationSlide -> pure destinationSlide
                  Nothing -> throwError (404, "Destination not exists")

  case res of
       Left (code, message) -> finishWithError code message
       Right destinationSlide ->
         copyTree sourcePath destinationPath destinationSlide

  where
    copyTree sourcePath destinationPath destinationSlide = do
      with db2 $ runPersist $ do
        transactionSave

        slideTree@(Node (_, topSlide) _) <-
          getTree $ fromIntegral $ last sourcePath

        (oldSourceId, newId) <- treeToCopyTransaction slideTree
        let (A.Array answers) = diagSlideAnswers destinationSlide

        let hasNextSlide = flip Vector.findIndex answers $ \(A.Object answer) ->
                           case HM.lookup "nextSlide" answer of
                             Just (A.Number n) ->
                               (floor n :: Int64) == oldSourceId
                             _ -> False

        let newAnswers =
                case hasNextSlide of
                  Just index ->
                      let (A.Object answer) = answers ! index
                          answer' = A.Object $
                                    HM.insert "nextSlide"
                                              (numberFromInt newId)
                                              answer
                      in Vector.toList $ answers // [(index, answer')]
                  Nothing ->
                      Vector.toList $ Vector.snoc answers $ A.Object $
                      HM.fromList [ ("text", A.String "")
                                  , ("header", A.String $
                                               diagSlideHeader topSlide)
                                  , ("action", A.Object $ HM.fromList [])
                                  , ("nextSlide", numberFromInt newId)
                                  ]

        update (mkKey $ last destinationPath)
               [ DiagSlideAnswers =. A.toJSON newAnswers ]

        transactionSave

      writeJSON ()

moveOrCopyDiagSlide MoveDiagSlide = do
  (sourcePath, destinationPath) <- getPasteParams
  -- sourcePath and destinationPath are untrusted arrays of DiagSlide
  -- identifiers, so we can use only last ids to build real entire trees
  -- from database.

  res <-
    runExceptT $ do
      when (null sourcePath) $ throwError (400, "Source is empty")
      when (null destinationPath) $ throwError (400, "Destination is empty")

      when (sourcePath == destinationPath) $
           throwError (400, "Unable to cut&paste to the same point")

      let sourceId = last sourcePath
      let destinationId = (fromIntegral $ last destinationPath) :: Int64

      sourceSubTree <- lift (with db2 $ runPersist $ getTree sourceId)

      when (isJust $ find ((== destinationId) . fst) sourceSubTree) $
           throwError (404, "Destination inside source")

      sourceSlide' <- lift $ with db2 $ runPersist $ getDiagSlide sourceId
      when (isNothing sourceSlide') $
           throwError (404, "Source not exists")

      let Just sourceSlide = sourceSlide'

      destinationSlide' <- lift $ with db2 $ runPersist $
                          getDiagSlide $ last destinationPath
      when (isNothing destinationSlide') $
           throwError (404, "Destination not exists")
      let Just destinationSlide = destinationSlide'

      parent <- if diagSlideIsRoot sourceSlide
                  then pure $ Right Nothing
                  else lift $ with db2 $ runPersist $ getParentSlide sourceId

      case parent of
        Left (code, message) -> throwError (code, message)
        Right parent'        -> pure (parent', destinationSlide)

  case res of
    Left (code, message) -> finishWithError code message
    Right (parent, destinationSlide) ->
      moveTree parent (last sourcePath) (last destinationPath) destinationSlide

  where
    getParentSlide childId = do
      parentId <- getParentId childId
      case parentId of
        Just parentId' ->
          do parentSlide <- getDiagSlide $ fromIntegral parentId'
             case parentSlide of
               Just parentSlide' ->
                 pure $ Right $ Just (parentId', parentSlide')
               Nothing ->
                 pure $ Left (404, "Unable to get parent slide for "
                                   ++ show parentId')

        Nothing -> pure $ Left (404, "Unable to get parent identifier for  "
                                    ++ show childId)


    getParentId sourceId = do
      parentId <- rawSql
                 "select id from \
                 \ (select id, json_array_elements(answers) \
                 \ as answer \
                 \ from \"DiagSlide\") as answers \
                 \ where (answer->>'nextSlide')::int = ?"
                 [PersistInt64 $ fromIntegral sourceId]
                 :: MonadIO m => ReaderT SqlBackend m [Single Int64]

      case parentId of
        (Single a : _) -> pure $ Just a
        _              -> pure Nothing

    moveTree parent sourceId destinationId destinationSlide = do
      let (A.Array destinationAnswers) = diagSlideAnswers destinationSlide

      with db2 $ runPersist $ do

        transactionSave

        newDestinationAnswers <-
          case parent of
            Just (parentId, parentSlide) ->
              -- move from one branch to another
              do let (A.Array answers) = diagSlideAnswers parentSlide
                 let (newParentAnswers, parentAnswerForSource) =
                       flip Vector.partition answers $ \(A.Object answer) ->
                         case HM.lookup "nextSlide" answer of
                           Just (A.Number sid) -> sourceId /= floor sid
                           _                   -> True

                 -- remove source slide from parent slide
                 update (mkKey $ fromIntegral parentId)
                        [ DiagSlideAnswers =. A.toJSON
                                              (Vector.toList newParentAnswers) ]

                 -- TODO FIXME parentAnswerForSource can be empty

                 pure $ Vector.toList $
                      Vector.snoc destinationAnswers $
                      Vector.head parentAnswerForSource

            Nothing ->
              -- move from root
              do update (mkKey sourceId)
                        [ DiagSlideIsRoot =. False ]

                 pure $ Vector.toList $
                      Vector.snoc destinationAnswers $
                      A.Object $
                      HM.fromList [ ("text", A.String "")
                                  , ("header", A.String $
                                               diagSlideHeader destinationSlide)
                                  , ("action", A.Object $ HM.fromList [])
                                  , ("nextSlide", numberFromInt $
                                                  fromIntegral sourceId)
                                  ]

        -- add source slide to destination
        update (mkKey destinationId)
               [ DiagSlideAnswers =. A.toJSON newDestinationAnswers ]

        transactionSave

      writeJSON ()
