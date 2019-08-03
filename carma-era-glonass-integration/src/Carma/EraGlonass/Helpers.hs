{-# LANGUAGE FlexibleContexts, OverloadedStrings, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces, RankNTypes, GADTs #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds #-}

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
import           Control.Concurrent.STM.TVar

import           Control.Exception
                   ( SomeException (SomeException)
                   , displayException
                   )

import           Database.Persist.Sql (SqlBackend, fromSqlKey)

import           Servant ((:<|>) ((:<|>)))

import           Carma.Monad.STM
import           Carma.Monad.Thread
import           Carma.Monad.LoggerBus
import           Carma.Monad.PersistentSql
import           Carma.Monad.Clock
import           Carma.Utils.TypeSafe.TypeFamilies (OneOf)
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.EraGlonass.Instances ()
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
  go = do
    ctime <- getCurrentTime
    void $ inBackground $ runSqlInTime (insert $ record ctime) >>= resolve

  bodyType = getFailureBodyType (Proxy :: Proxy bodyType)

  record ctime =
    CaseEraGlonassFailure
      { caseEraGlonassFailureCtime            = ctime
      , caseEraGlonassFailureIntegrationPoint = integrationPoint
      , caseEraGlonassFailureRequestId        = Nothing
      , caseEraGlonassFailureComment          = Just comment

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

  model = typeName (Proxy :: Proxy CaseEraGlonassFailure) :: Text

  resolve (Right failureId) = logErrorS [qm| {integrationPoint} |] [qms|
    Failure incident record is successfully saved to the database:
    "{model}" id#{fromSqlKey failureId}.
  |]

  resolve (Left (SomeException e)) = logErrorS [qm| {integrationPoint} |] [qms|
    Failed to save failure incident record into database,
    it is failed with exception: {displayException e}
  |]
