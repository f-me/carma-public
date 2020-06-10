{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns, ExplicitNamespaces, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds, GADTs #-}

module Carma.EraGlonass.Helpers
     ( inBackground
     , runSqlInTime
     , (#), type (#)

     , FailureBodyType (..)
     , GetFailureBodyType (..)
     , FailureBody (..)
     , reportToHouston
     ) where


import           Data.Proxy
import           Data.Aeson
import           Data.Text (Text)
import           Text.InterpolatedString.QM

import           Control.Monad (void)
import           Control.Monad.Reader (MonadReader, ReaderT, asks)
import           Control.Monad.Catch (MonadCatch (catch))
import           Control.Monad.Trans.Class (lift)
import           Control.Applicative ((<|>))
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Lifted (fork)

import           Control.Exception
                   ( SomeException (SomeException)
                   , displayException
                   )

import           Database.Persist ((==.), (=.), selectFirst, insert, update)
import           Database.Persist.Types (Entity (..))
import           Database.Persist.Sql (SqlBackend, fromSqlKey)
import           Database.Persist.Types (SelectOpt (Desc))

import           Servant ((:<|>) ((:<|>)))

import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus.Class
import           Carma.Monad.Clock
import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.TypeFamilies (OneOf)
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.EraGlonass.Instances ()
import           Carma.EraGlonass.Instance.Persistent
import           Carma.EraGlonass.Model.CaseEraGlonassFailure.Persistent
import           Carma.EraGlonass.Types.AppContext (AppContext (..))
import           Carma.EraGlonass.Types.EGIntegrationPoint (EGIntegrationPoint)


type (#) = (:<|>)
(#) = (:<|>)
(#) :: a -> b -> (:<|>) a b
infixr 3 #
{-# INLINE (#) #-}


inBackground
  ::
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadThread m
   , MonadCatch m
   , MonadSTM m
   )
  => m ()
  -> m ()

inBackground m = do
  counter <- asks backgroundTasksCounter
  atomically $ modifyTVar' counter succ
  void $ fork $ do
    m `catch` \(SomeException e) ->
      void $ fork $ logErrorS "BackgroundTask" [qms|
        Some background task is unexpectedly failed with exception:
        {displayException e}
      |]

    atomically $ modifyTVar' counter pred


runSqlInTime
  :: (MonadReader AppContext m, MonadPersistentSql m)
  => ReaderT SqlBackend m a
  -> m (Either SomeException a)

runSqlInTime m = asks dbRequestTimeout >>= flip runSqlTimeout m


data FailureBodyType
   = FailureNoBodyType
   | FailureRequestBodyType
   | FailureResponseBodyType
     deriving Eq

-- | Lifts "FailureBodyType" from type-level to term-level.
class GetFailureBodyType (a :: FailureBodyType) where
  getFailureBodyType :: Proxy a -> FailureBodyType
instance GetFailureBodyType 'FailureNoBodyType where
  getFailureBodyType Proxy = FailureNoBodyType
instance GetFailureBodyType 'FailureRequestBodyType where
  getFailureBodyType Proxy = FailureRequestBodyType
instance GetFailureBodyType 'FailureResponseBodyType where
  getFailureBodyType Proxy = FailureResponseBodyType

data FailureBody (t :: FailureBodyType) where
     FailureWithoutBody :: FailureBody 'FailureNoBodyType

     FailureWithBody
       :: OneOf t '[ 'FailureRequestBodyType, 'FailureResponseBodyType ]
       => Value
       -> FailureBody t

-- | Report about some failure by adding record of it into database.
--
-- If a record with the same failure message exists it will just patch existing
-- record, updating its counter and last modification date field.
--
-- It reports in background, report may fail (such a failure will be logged).
reportToHouston
  :: forall m bodyType
   .
   ( MonadReader AppContext m
   , MonadLoggerBus m
   , MonadClock m
   , MonadCatch m
   , MonadThread m
   , MonadPersistentSql m
   , MonadSTM m
   , GetFailureBodyType bodyType
   )
  => FailureBody bodyType
  -> EGIntegrationPoint
  -> Text -- ^ Textuar failure comment
  -> m ()

reportToHouston body integrationPoint comment = go where
  go :: m ()
  go = void $ inBackground $ runSqlInTime transaction >>= resolve

  transaction :: ReaderT SqlBackend m (Bool, CaseEraGlonassFailureId)
  transaction = do
    ctime <- lift getCurrentTime
    let CaseEraGlonassFailure {..} = record ctime

    previousRecordOfThisFailure <-
      selectFirst
        [ CaseEraGlonassFailureIntegrationPoint ==.
            caseEraGlonassFailureIntegrationPoint

        , CaseEraGlonassFailureRequestId   ==. caseEraGlonassFailureRequestId
        , CaseEraGlonassFailureComment     ==. caseEraGlonassFailureComment
        , CaseEraGlonassFailureRequestBody ==. caseEraGlonassFailureRequestBody

        , CaseEraGlonassFailureResponseBody ==.
            caseEraGlonassFailureResponseBody
        ]
        [ -- Trying to add a repeat to latest (by creation time) failure
          Desc CaseEraGlonassFailureCtime
        ]

    case previousRecordOfThisFailure of
         Nothing -> (,) <$> pure True <*> insert (record ctime)

         Just Entity { entityKey = prevFailureId
                     , entityVal = CaseEraGlonassFailure
                         { caseEraGlonassFailureRepeats = repeats }
                     } ->

           (False, prevFailureId) <$ update prevFailureId
             [ CaseEraGlonassFailureRepeats =.
                 (repeats <|> Just mempty) <&> (<> pure ctime)
             ]

  record ctime =
    CaseEraGlonassFailure
      { caseEraGlonassFailureCtime            = ctime
      , caseEraGlonassFailureIntegrationPoint = integrationPoint
      , caseEraGlonassFailureRequestId        = Nothing
      , caseEraGlonassFailureComment          = Just comment
      , caseEraGlonassFailureRepeats          = Nothing

      , caseEraGlonassFailureRequestBody = case body of
          FailureWithoutBody -> Nothing
          FailureWithBody x
            | bodyType == FailureRequestBodyType -> Just x
            | otherwise -> Nothing

      , caseEraGlonassFailureResponseBody = case body of
          FailureWithoutBody -> Nothing
          FailureWithBody x
            | bodyType == FailureResponseBodyType -> Just x
            | otherwise -> Nothing
      }

  bodyType = getFailureBodyType (Proxy :: Proxy bodyType)
  model = typeName (Proxy :: Proxy CaseEraGlonassFailure) :: Text

  resolve (Right (isNew, failureId)) = logErrorS [qm|{integrationPoint}|] [qms|
    Failure incident record is successfully saved to the database:
    "{model}" id#{fromSqlKey failureId}
    ({ if isNew
          then "new record have been created"
          else "found previous record of this failure,\
               \ that record have been updated by adding current timestamp\
               \ to the list of repeats" :: Text }).
  |]

  resolve (Left (SomeException e)) = logErrorS [qm|{integrationPoint}|] [qms|
    Failed to save failure incident record into database,
    it is failed with exception: {displayException e}
  |]
