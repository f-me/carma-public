{-# LANGUAGE ScopedTypeVariables #-}

module Carma.Model.FieldPermission where

import           Prelude hiding (pred)
import           Data.Text hiding (filter)
import           Data.Model
import           Data.Typeable
import           Data.Maybe (fromJust)

import           Database.PostgreSQL.Simple as PG
import qualified Data.HashMap.Strict as HM

import           Data.Aeson (Result(..))

import           Data.Model.Sql
import           Data.Model.Patch
import           Data.Model.View
import           Carma.Model.Role (Role)

data FieldPermission = FieldPermission
  {ident :: PK Int FieldPermission ""
  ,role  :: F (IdentI Role)  "role"  "Роль"
  ,model :: F Text          "model" "Модель"
  ,field :: F Text          "field" "Внутреннее название поля"
  ,r     :: F Bool          "r"     "Доступно для чтения"
  ,w     :: F Bool          "w"     "Доступно для записи"
  } deriving Typeable


instance Model FieldPermission where
  type TableName FieldPermission = "FieldPermission"
  modelInfo = mkModelInfo FieldPermission ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing

stripReadPatch :: (Model m, Model (Parent m))
               => Connection
               -> [IdentI Role]
               -> Patch m
               -> IO (Patch m)
stripReadPatch = stripPatch $ eq r True

stripWritePatch :: (Model m, Model (Parent m))
                => Connection
                -> [IdentI Role]
                -> Patch m
                -> IO (Patch m)
stripWritePatch = stripPatch $ eq w True

stripPatch :: forall m.(Model m, Model (Parent m))
           => (SqlP FieldPermission Bool)
           -> Connection
           -> [IdentI Role]
           -> Patch m
           -> IO (Patch m)
stripPatch q c rs p = do
  mInfo <- stripFields q c rs (modelInfo :: ModelInfo m)
  let n = HM.keys $ modelFieldsMap mInfo
  return $ Patch $ HM.filterWithKey (\k _ -> k `elem` n) $ untypedPatch p

stripReadFields :: (Model m, Model (Parent m))
                => Connection
                -> [IdentI Role]
                -> ModelInfo m
                -> IO (ModelInfo m)
stripReadFields = stripFields $ eq r True

stripWriteFields :: (Model m, Model (Parent m))
                 => Connection
                 -> [IdentI Role]
                 -> ModelInfo m
                 -> IO (ModelInfo m)
stripWriteFields = stripFields $ eq w True

stripFields :: (Model m, Model (Parent m))
            => (SqlP FieldPermission Bool)
            -> PG.Connection
            -> [IdentI Role]
            -> (ModelInfo m)
            -> IO (ModelInfo m)
stripFields q c rs mInfo = do
  fs <- retrieveFields q c rs mInfo
  let filteredFields = filter (\v -> elem (fd_name v) fs)
  return $ mInfo
    { modelFieldsMap  = HM.filterWithKey (\k _ -> elem k fs) $
                          modelFieldsMap mInfo
    , modelFields     = filteredFields $ modelFields mInfo
    , modelOnlyFields = filteredFields $ modelOnlyFields mInfo
    }

retrieveFields :: forall m.(Model m, Model (Parent m))
               => (SqlP FieldPermission Bool)
               -> PG.Connection
               -> [IdentI Role]
               -> ModelInfo m
               -> IO [Text]
retrieveFields q c rs mInfo = do
  fs <- getFields $ modelName mInfo
  fsp <- maybe (return []) getFields $ parentName mInfo
  return $ fs ++ fsp
  where
    pred n = field :. sql_in role rs :. model `eq` n :. q
    getFields info
      = selectPatch (pred info) c >>= \case
        Error s   -> fail $
                     "Can't check permission fields retrieve failed with: " ++ s
        Success p -> return $ Prelude.map (fromJust . flip get field) p
