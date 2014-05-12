{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model
  ( Ident(..), IdentI, IdentT
  , Model(..)
  , NoParent
  , ModelInfo(..), mkModelInfo
  , Field(..), F, PK
  , FOpt
  , FieldDesc(..)
  -- Field accessor introspection
  , buildFieldDesc, fieldName, fieldDesc, fieldType
  -- Existential field accessors and introspection
  , FA(..), fieldNameE, fieldTypesQ
  -- from Data.Model.View.Types
  , ModelView(..)
  , hasNoParent
  , GetModelFields(..)
  , withLegacyName -- imported from Data.Model.Types
  ) where


import Control.Applicative
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson.Types as Aeson

import Database.PostgreSQL.Simple.FromRow   (RowParser,field)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Data.Dynamic
import GHC.TypeLits

import Language.Haskell.TH

import Data.Model.TH
import Data.Model.Types


mkModelInfo
  :: forall m ctr pkTy pkNm pkDesc
  . (Model m, Model (Parent m), GetModelFields m ctr, SingI pkNm)
  => ctr -> (m -> F (Ident pkTy m) pkNm pkDesc)
  -> ModelInfo m
mkModelInfo ctr pk =
  let parentFlds
        = if hasNoParent (undefined :: m)
          then []
          else modelFields (modelInfo :: ModelInfo (Parent m))
      modelOnlyFlds = unWrap (getModelFields ctr :: [FieldDesc] :@ m)
      modelFlds = parentFlds ++ modelOnlyFlds
  in ModelInfo
    { modelName      = T.pack $ show $ typeOf (undefined :: m)
    , legacyModelName= Nothing
    , tableName      = T.pack $ fromSing (sing :: Sing (TableName m))
    , primKeyName    = fieldName pk
    , modelFields    = modelFlds
    , modelOnlyFields= modelOnlyFlds
    , modelFieldsMap = HashMap.fromList [(fd_name f, f) | f <- modelFlds]
    }

hasNoParent :: forall m.(Model (Parent m)) => m -> Bool
hasNoParent _ = typeOf (undefined :: Parent m) == typeOf (undefined :: NoParent)


class (SingI (TableName m), Typeable m, Typeable (Parent m)) => Model m where
  type TableName m :: Symbol

  type Parent m
  type Parent m = NoParent

  modelInfo :: ModelInfo m
  modelView :: Text -> Maybe (ModelView m)

  -- | String-to-ident mappings for the model.
  idents    :: HashMap.HashMap String (IdentI m)
  idents    =  HashMap.empty


instance (Model m, Show t) => Show (Ident t m) where
  show (Ident x :: Ident t m) = "Ident " ++ modelName ++ " " ++ show x
    where
      modelName = show $ typeOf (undefined :: m)


data NoParent deriving Typeable
instance Model NoParent where
  type TableName NoParent = "(undefined)"
  type Parent NoParent = NoParent
  modelInfo = error "ModelInfo NoParent"
  modelView = error "ModelView NoParent"


fieldName
  :: forall m t name desc
  . SingI name => (m -> Field t (FOpt name desc)) -> Text
fieldName _ = T.pack $ fromSing (sing :: Sing name)

fieldDesc
  :: forall m t name desc
  . SingI desc => (m -> Field t (FOpt name desc)) -> Text
fieldDesc _ = T.pack $ fromSing (sing :: Sing desc)

fieldType
  :: forall m t name desc
  . Typeable t => (m -> Field t (FOpt name desc)) -> TypeRep
fieldType _ = typeOf (undefined :: t)


fieldNameE :: FA m -> Text
fieldNameE (FA f) = fieldName f


-- | Expand to N-tuple of field types, where N matches length of
-- 'identifiers'.
fieldTypesQ :: [FA m] -> Q Type
fieldTypesQ fields =
    return $ foldl AppT (TupleT (length fields)) $
           map (\(FA f) -> (typeRepToType $ fieldType f)) fields


class GetModelFields m ctr where
  getModelFields :: ctr -> Wrap m [FieldDesc]

instance
    (GetModelFields m ctr, SingI nm, SingI desc, DefaultFieldView t
    ,FromJSON t, ToJSON t, FromField t, ToField t, Typeable t, PgTypeable t)
    => GetModelFields m (Field t (FOpt nm desc) -> ctr)
  where
    getModelFields f = Wrap
      $ buildFieldDesc (undefined :: m -> Field t (FOpt nm desc))
      : unWrap (getModelFields (f Field) :: [FieldDesc] :@ m)

instance GetModelFields m m where
  getModelFields _ = Wrap []

buildFieldDesc :: forall m t nm desc.
  (SingI nm, SingI desc, DefaultFieldView t, PgTypeable t
  ,FromJSON t, ToJSON t, FromField t, ToField t, Typeable t)
  => (m -> Field t (FOpt nm desc)) -> FieldDesc
buildFieldDesc _ =  FieldDesc
          {fd_name      = T.pack $ fromSing (sing :: Sing nm)
          ,fd_desc      = T.pack $ fromSing (sing :: Sing desc)
          ,fd_type      = typeOf   (undefined :: t)
          ,fd_parseJSON = \v -> toDyn <$> (parseJSON v :: Parser t)
          ,fd_toJSON    = \d -> toJSON  (fromJust $ fromDynamic d :: t)
          ,fd_fromField = toDyn <$> (field :: RowParser t)
          ,fd_toField   = \d -> toField (fromJust $ fromDynamic d :: t)
          ,fd_view      = defaultFieldView (const Field :: m -> F t nm desc)
          ,fd_pgType    = pgTypeOf (undefined :: t)
          }
