{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model
  ( Ident(..), IdentI, IdentT
  , Model(..)
  , ParentInfo(..)
  , ModelInfo(..), mkModelInfo
  , Field(..), FF, F, EF, PK
  , FOpt
  , FieldDesc(..)
  -- Field accessor introspection
  , buildFieldDesc, fieldName, fieldDesc, fieldType
  -- Existential field accessors and introspection
  , FA(..), fieldNameE, fieldTypesQ
  -- from Data.Model.View.Types
  , ModelView(..)
  , GetModelFields(..)
  , onlyDefaultFields
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

import Data.Singletons
import Data.Singletons.TypeLits

import Language.Haskell.TH

import Data.Model.TH
import Data.Model.Types


mkModelInfo
  :: forall m ctr pkTy pkNm pkDesc
  . (Model m, GetModelFields m ctr, SingI pkNm)
  => ctr -> (m -> F (Ident pkTy m) pkNm pkDesc)
  -> ModelInfo m
mkModelInfo ctr pk = case parentInfo :: ParentInfo m of
  NoParent      -> mInf
  ExParent pInf ->
    let pFields
          = filter ((/= primKeyName pInf) . fd_name) (modelFields pInf)
          ++ modelFields mInf
    in mInf
      {parentName     = Just $ modelName pInf
      ,modelFields    = pFields
      ,modelFieldsMap = HashMap.fromList [(fd_name f, f) | f <- pFields]
      }
  where
    mFields = unWrap (getModelFields ctr :: [FieldDesc] :@ m)
    mInf = ModelInfo
      { modelName       = T.pack $ show $ typeOf (undefined :: m)
      , parentName      = Nothing
      , legacyModelName = Nothing
      , tableName       = T.pack $ fromSing (sing :: Sing (TableName m))
      , primKeyName     = fieldName pk
      , modelFields     = mFields
      , modelOnlyFields = mFields
      , modelFieldsMap  = HashMap.fromList [(fd_name f, f) | f <- mFields]
      , modelCRUD       = Nothing
      }

-- This existential type allows to decide in runtime wether model has parent
-- or not. It is also allows to use this parent: after pattern-matching on
-- ExParnent the `Model p` instance is brought into the current context.
data ParentInfo m where
-- TODO: it is possible to constrain NoParent to be used only in classes with
-- `type Parent m = NoParent` but this will require to explicitly state
-- `type Parent` and `parentInfo` even for not inherited models.
--  NoParent :: (Model m, p ~ Parent m, p ~ NoParent) => [p] -> ParentInfo m
  NoParent :: ParentInfo m
  ExParent :: (Model m, p ~ Parent m, Model p) => ModelInfo p -> ParentInfo m


class (SingI (TableName m), Typeable m, Typeable (Parent m)) => Model m where
  type TableName m :: Symbol

  type Parent m
  type Parent m = NoParent

  parentInfo :: ParentInfo m
  parentInfo = NoParent

  modelInfo   :: ModelInfo m
  modelView   :: Text -> Maybe (ModelView m)

  -- | String-to-ident mappings for the model.
  idents    :: HashMap.HashMap String (IdentI m)
  idents    =  HashMap.empty


-- FIXME: we can replace NoParent with ()
data NoParent deriving Typeable
instance Model NoParent where
  type TableName NoParent = "(undefined)"
  modelInfo = error "ModelInfo NoParent"
  modelView = error "ModelView NoParent"

fieldName
  :: forall m t name desc app
  . SingI name => (m -> Field t (FOpt name desc app)) -> Text
fieldName _ = T.pack $ fromSing (sing :: Sing name)

fieldDesc
  :: forall m t name desc app
  . SingI desc => (m -> Field t (FOpt name desc app)) -> Text
fieldDesc _ = T.pack $ fromSing (sing :: Sing desc)

fieldType
  :: forall m t name desc app
  . Typeable t => (m -> Field t (FOpt name desc app)) -> TypeRep
fieldType _ = typeOf (undefined :: t)


fieldNameE :: FA m -> Text
fieldNameE (FA f) = fieldName f

onlyDefaultFields :: [FieldDesc] -> [FieldDesc]
onlyDefaultFields = filter isDefault
  where
    isDefault FieldDesc{..} = True
    isDefault _             = False

-- | Expand to N-tuple of field types, where N matches length of
-- 'identifiers'.
fieldTypesQ :: [FA m] -> Q Type
fieldTypesQ fields =
    return $ foldl AppT (TupleT (length fields)) $
           map (\(FA f) -> (typeRepToType $ fieldType f)) fields


class GetModelFields m ctr where
  getModelFields :: ctr -> Wrap m [FieldDesc]

instance
    ( GetModelFields m ctr
    , Typeable t, PgTypeable t
    , DefaultFieldView t
    , FromJSON t, ToJSON t
    , FromField t, ToField t
    , SingI nm, SingI desc)

    => GetModelFields m (F t nm desc -> ctr)
  where
    getModelFields f  = Wrap
      $ buildFieldDesc (undefined :: m -> F t nm desc)
      : unWrap (getModelFields (f Field) :: [FieldDesc] :@ m)

-- instance
--     ( GetModelFields m ctr
--     , Typeable (Ident t m), PgTypeable (Ident t m)
--     , DefaultFieldView (Ident t m) "pk"
--     , FromJSON (Ident t m), ToJSON (Ident t m)
--     , FromField (Ident t m), ToField (Ident t m)
--     , SingI desc)
--     => GetModelFields m (PK t m desc -> ctr)
--   where
--     getModelFields f  = Wrap
--       $ buildFieldDesc (undefined :: m -> PK t m desc)
--       : unWrap (getModelFields (f Field) :: [FieldDesc] :@ m)



instance
    (GetModelFields m ctr, SingI nm, SingI desc
    ,DefaultFieldView t
    ,ToJSON t, FromJSON t, Typeable t)
    => GetModelFields m (EF t nm desc -> ctr)
  where
    getModelFields f = Wrap
      $ EFieldDesc
        { fd_name   = T.pack $ fromSing (sing :: Sing nm)
        , fd_desc   = T.pack $ fromSing (sing :: Sing desc)
        , fd_type   = typeOf   (undefined :: t)
        , fd_toJSON = \d -> toJSON  (fromJust $ fromDynamic d :: t)
        , fd_parseJSON = \v -> toDyn <$> (parseJSON v :: Parser t)
        , fd_view   = defaultFieldView (const Field :: m -> EF t nm desc)
        }
      : unWrap (getModelFields (f Field) :: [FieldDesc] :@ m)

instance GetModelFields m m where
  getModelFields _ = Wrap []

buildFieldDesc :: forall m t nm desc app.
  (SingI nm, SingI desc, FieldKindSing app, DefaultFieldView t, PgTypeable t
  ,FromJSON t, ToJSON t, FromField t, ToField t, Typeable t)
  => (m -> FF t nm desc app) -> FieldDesc
buildFieldDesc _ =  FieldDesc
  {fd_name      = T.pack $ fromSing (sing :: Sing nm)
  ,fd_desc      = T.pack $ fromSing (sing :: Sing desc)
  ,fd_type      = typeOf   (undefined :: t)
  ,fd_parseJSON = \v -> toDyn <$> (parseJSON v :: Parser t)
  ,fd_toJSON    = \d -> toJSON  (fromJust $ fromDynamic d :: t)
  ,fd_fromField = toDyn <$> (field :: RowParser t)
  ,fd_toField   = \d -> toField (fromJust $ fromDynamic d :: t)
  ,fd_view      = defaultFieldView (const Field :: m -> FF t nm desc app)
  ,fd_pgType    = pgTypeOf (undefined :: t)
  }
