{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies, DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces, DeriveGeneric, TypeInType #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}

-- To add docs for every type or function defined in the module.
{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

-- | Additional types for VIN synchronizer.
module Carma.EraGlonass.VinSynchronizer.Types
     ( type OneOrTwoNonEmptyLists (..)
     , getFirstNonEmptyList
     , getSecondNonEmptyList
     , oneOrTwoNonEmptyListsLengths

     , type BodyParseFailure (..)
     , type FailureScenario (..)

     , type VinToUnmarkModelId (..)
     , type VINsToUnmark (..)
     , defaultVINsToUnmark
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import           Data.Monoid ((<>))
import           Data.Either.Combinators (mapLeft)
import           Data.Text (type Text)
import           Data.String (type IsString (fromString))
import           Text.InterpolatedString.QM
import           Data.List.NonEmpty (type NonEmpty)
import           Data.Aeson (type Value)
import qualified Data.Attoparsec.Text as P

import           Control.Applicative ((<|>))
import           Control.Exception (type Exception)

import           Database.Persist.Sql (type SqlBackend, toSqlKey, fromSqlKey)
import           Database.Persist.Types (type PersistValue (..))

import           Database.Persist.Class
                   ( type PersistField (..)
                   , type ToBackendKey
                   )

import           Carma.Utils.TypeSafe.TypeFamilies
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Model.Contract.Persistent
import           Carma.EraGlonass.Types.EGVin (EGVin)
import           Carma.EraGlonass.Model.EraGlonassSynchronizedContract.Persistent


-- | Helper type for VINs unmarking.
data OneOrTwoNonEmptyLists first second
   = FirstNonEmptyList  (NonEmpty first)
   | SecondNonEmptyList (NonEmpty second)
   | BothNonEmptyLists  (NonEmpty first) (NonEmpty second)
     deriving (Show, Eq)


getFirstNonEmptyList :: OneOrTwoNonEmptyLists a b -> Maybe (NonEmpty a)
getFirstNonEmptyList (FirstNonEmptyList  x  ) = Just x
getFirstNonEmptyList (SecondNonEmptyList   _) = Nothing
getFirstNonEmptyList (BothNonEmptyLists  x _) = Just x

getSecondNonEmptyList :: OneOrTwoNonEmptyLists a b -> Maybe (NonEmpty b)
getSecondNonEmptyList (FirstNonEmptyList  _  ) = Nothing
getSecondNonEmptyList (SecondNonEmptyList   x) = Just x
getSecondNonEmptyList (BothNonEmptyLists  _ x) = Just x


oneOrTwoNonEmptyListsLengths :: OneOrTwoNonEmptyLists a b -> (Word, Word)
oneOrTwoNonEmptyListsLengths (FirstNonEmptyList first) =
  ( fromIntegral $ length first
  , minBound
  )
oneOrTwoNonEmptyListsLengths (SecondNonEmptyList second) =
  ( minBound
  , fromIntegral $ length second
  )
oneOrTwoNonEmptyListsLengths (BothNonEmptyLists first second) =
  ( fromIntegral $ length first
  , fromIntegral $ length second
  )


data BodyParseFailure
   = ResponseParseFailure
   { errorMessage :: String
   , responseBody :: Value
   } deriving Show

instance Exception BodyParseFailure


newtype FailureScenario
      = FailureScenario
      { failureMessage :: String
      } deriving Show

instance Exception FailureScenario


-- | Helper type to branch model in a single SQL query.
data VinToUnmarkModelId
   = ContractIdToUnmark ContractId
   | EraGlonassSynchronizedContractIdToUnmark EraGlonassSynchronizedContractId
     deriving (Eq, Show, Generic)

type family VinToUnmarkModelIdToSymbol
            (constructor :: Key a -> VinToUnmarkModelId)
                         :: Symbol where

  VinToUnmarkModelIdToSymbol 'ContractIdToUnmark =
    "ContractIdToUnmark"
  VinToUnmarkModelIdToSymbol 'EraGlonassSynchronizedContractIdToUnmark =
    "EraGlonassSynchronizedContractIdToUnmark"

-- | Returns proven constructor name from term-level value.
vinToUnmarkModelIdConstructorNameByValue
  :: forall t s. (t ~ VinToUnmarkModelId, IsString s) => t -> s

vinToUnmarkModelIdConstructorNameByValue = \case
  ContractIdToUnmark _ ->
    vinToUnmarkModelIdConstructorToItsName $
      Proxy @('ContractIdToUnmark :: ContractId -> t)

  EraGlonassSynchronizedContractIdToUnmark _ ->
    vinToUnmarkModelIdConstructorToItsName $
      Proxy @('EraGlonassSynchronizedContractIdToUnmark ::
               EraGlonassSynchronizedContractId -> t)

-- | Returns proven constructor name from term-level constructor.
vinToUnmarkModelIdConstructorNameByConstructor
  ::
   ( t ~ VinToUnmarkModelId
   , OneOf model '[ Contract, EraGlonassSynchronizedContract ]
   , ToBackendKey SqlBackend model
   , IsString s
   )
  => (Key model -> t)
  -> s

vinToUnmarkModelIdConstructorNameByConstructor f =
  vinToUnmarkModelIdConstructorNameByValue $ f $ toSqlKey minBound

-- | Returns proven constructor name from type-level constructor.
class VinToUnmarkModelIdConstructorToItsName
      (a :: Key model -> VinToUnmarkModelId) where

  vinToUnmarkModelIdConstructorToItsName :: IsString s => Proxy a -> s

instance ( cs ~ VinToUnmarkModelIdToSymbol c
         , ConstructorName (Rep VinToUnmarkModelId) cs ~ 'Just cs
         , KnownSymbol cs
         ) => VinToUnmarkModelIdConstructorToItsName c where

  vinToUnmarkModelIdConstructorToItsName Proxy =
    fromString $ symbolVal $ Proxy @cs

parseVinToUnmarkModelId :: P.Parser VinToUnmarkModelId
parseVinToUnmarkModelId = go where
  go = (contract <|> egSyncContract) <* P.endOfInput

  contract       = f ContractIdToUnmark
  egSyncContract = f EraGlonassSynchronizedContractIdToUnmark

  f wrap = parser where
    parser = wrap . toSqlKey <$> (P.string constructorStr *> P.decimal)
    constructorStr = vinToUnmarkModelIdConstructorNameByConstructor wrap <> " "

instance t ~ VinToUnmarkModelId => PersistField VinToUnmarkModelId where
  toPersistValue (constructor :: t) = go where
    f (id' :: Key record) (pfx :: Text) =
      toPersistValue $ ((pfx <> " ") <>) $ fromString $ show $ fromSqlKey id'

    go =
      case constructor :: t of
           ContractIdToUnmark id' ->
             f id' $ vinToUnmarkModelIdConstructorNameByValue constructor
           EraGlonassSynchronizedContractIdToUnmark id' ->
             f id' $ vinToUnmarkModelIdConstructorNameByValue constructor

  fromPersistValue = \case
    PersistText x -> mapLeft fromString $ P.parseOnly parseVinToUnmarkModelId x
    x             -> Left [qm| Expected VinToUnmarkModelId, received: {x} |]


data VINsToUnmark
   = VINsToUnmark
   { totalVINsToUnmarkCount     :: Word
   , contractVINsToUnmark       :: [(ContractId, EGVin)]
   , contractVINsToUnmarkCount  :: Word
   , ephemeralVINsToUnmark      :: [(EraGlonassSynchronizedContractId, EGVin)]
   , ephemeralVINsToUnmarkCount :: Word
   } deriving (Eq, Show)

defaultVINsToUnmark :: VINsToUnmark
defaultVINsToUnmark
  = VINsToUnmark
  { totalVINsToUnmarkCount     = minBound
  , contractVINsToUnmark       = mempty
  , contractVINsToUnmarkCount  = minBound
  , ephemeralVINsToUnmark      = mempty
  , ephemeralVINsToUnmarkCount = minBound
  }
