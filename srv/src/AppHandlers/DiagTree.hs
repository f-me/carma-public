{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppHandlers.DiagTree
    ( diagInfo
    , diagHistory
    , retryQuestion

    , MoveOrCopyDiagSlide (..)
    , moveOrCopyDiagSlide
    )

where

import           Control.Monad                       (forM, mzero)
import           Control.Monad.IO.Class              (MonadIO)
import           Control.Monad.Reader                (ReaderT)
import           Data.Aeson                          as A
import qualified Data.HashMap.Strict                 as HM
import           Data.List                           (find)
import           Data.Tree
import           Data.Vector                         ((!), (//))
import qualified Data.Vector                         as Vector
import           Database.Persist
import           Database.Persist.Sql                ( fromSqlKey
                                                     , transactionSave
                                                     )
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
  } deriving (Show)

instance FromJSON CopyMoveOperation where
  parseJSON (Object o) =
    CopyMoveOperation <$> o .: "source" <*> o .: "destination"

  parseJSON _ = mzero


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
    Just entityDiagSlide -> return $ Just $ entityVal entityDiagSlide
    Nothing              -> return Nothing


-- | Get tree of @DiagSlide@s by a key
getTree
  :: ( BaseBackend backend ~ Database.Persist.Sql.Types.Internal.SqlBackend
     , PersistQueryRead backend
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
      return $ Node (fromIntegral key, diagSlide) children
    Nothing -> error $ "invalid id " ++ show key


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

treeToCopyTransaction tree =
  case tree of
    Node (oldKey, slide) [] -> do
      newKey <- insert slide
      return (oldKey, fromSqlKey newKey)
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
      return (oldKey, fromSqlKey newKey)


moveOrCopyDiagSlide :: MoveOrCopyDiagSlide -> AppHandler ()
moveOrCopyDiagSlide CopyDiagSlide = do
  body <- getJSONBody :: AppHandler CopyMoveOperation
  let sourcePath = source body
      destinationPath = destination body

  if null sourcePath
    then error "source is empty"
    else if null destinationPath
         then error "destination is empty"
         else if sourcePath == destinationPath
              then error "unable to copy to the same point"
              else do res <- with db2 $ runPersist $
                             getDiagSlide $ last destinationPath
                      case res of
                        Just destinationSlide ->
                          copyTree sourcePath destinationPath destinationSlide
                        Nothing -> error "destination is not exists"

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
  _ <- getJSONBody :: AppHandler CopyMoveOperation

  (_ :: [Entity DiagSlide]) <-
    with db2 $ runPersist $
      selectList [] [LimitTo 10]

  writeJSON ()
