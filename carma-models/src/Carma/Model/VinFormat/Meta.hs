{-|

VinFormat macros and meta helpers.

The idea is to list a set of annotated 'Contract' fields and generate
VinFormat model from it. Each source field @f@ is spliced into several
fields @fLoad@, @fRequired@ etc., depending on the annotation given
for that field. 'VFAccessor' type preserves information about how a
source field has is mapped to a group of produced VinFormat accessors.

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat.Meta
    ( ContractField(..), FF(..)
    , FormatFieldType(..)
    , mkVinFormat
    , VFAccessor(..), ParamAcc(..)
    )

where

import Data.Singletons
import Data.Singletons.TH
import Data.Typeable
import qualified GHC.TypeLits as GHC
import Language.Haskell.TH

import Data.Text as T (Text, unpack)
import Text.Printf
import Data.Vector (Vector)

import Data.Model

import Carma.Model.Contract (Contract)


-- | Existential wrapper for 'Contract' field accessors.
data ContractField where
    CF :: (Typeable t, GHC.SingI n, GHC.SingI d) =>
          (Contract -> F t n d) -> ContractField


fieldProjFormatter :: (ContractField -> String)
                   -> ContractField
                   -> String
                   -- ^ 'printf' format.
                   -> String
fieldProjFormatter proj cf format = printf format $ proj cf


ns :: TypeQ -> StrictTypeQ
ns = strictType (return NotStrict)


-- | Convert 'TypeRep' to Template Haskell 'Type'.
typeRepToType :: TypeRep -> Type
typeRepToType tr =
    let
        tyConToType tyCon = ConT $ mkName $ tyConName tyCon
        (conTr, trs)      = splitTyConApp tr
    in
      foldl (\t m -> AppT t (typeRepToType m)) (tyConToType conTr) trs


-- | VIN format field parameter.
--
-- Minimal definition requires 'ParamAcc', 'nameFormat' and
-- 'descFormat'.
class Typeable (ParamType a) => VinFieldParameter a where
    type ParamType a
    data ParamAcc a m

    nameFormat :: a -> String
    descFormat :: a -> String

    -- | Unique name of accessor and database column.
    name       :: a -> ContractField -> String
    desc       :: a -> ContractField -> String

    type ParamType a = Bool

    name a cf  = fieldProjFormatter (\(CF f) -> T.unpack $ fieldName f) cf $
                 nameFormat a
    desc a cf  = fieldProjFormatter (\(CF f) -> T.unpack $ fieldDesc f) cf $
                 descFormat a


mkAcc :: VinFieldParameter a =>
         a -> ContractField -> VarStrictTypeQ
mkAcc (a :: a) cf =
    varStrictType (mkName n) $ ns $
    [t|
     F $(return $ typeRepToType t)
     $(litT $ strTyLit n)
     $(litT $ strTyLit d)|]
    where
      n = name a cf
      d = desc a cf
      t = typeOf $ (undefined :: ParamType a)


data Load
instance VinFieldParameter Load where
    data ParamAcc Load m =
        forall n d. (GHC.SingI n, GHC.SingI d) =>
        LoadAcc (m -> F (ParamType Load) n d)
    nameFormat _ = "%sLoad"
    descFormat _ = "Загружать поле «%s»"


data Required
instance VinFieldParameter Required where
    data ParamAcc Required m =
        forall n d. (GHC.SingI n, GHC.SingI d) =>
        RequiredAcc (m -> F (ParamType Required) n d)
    nameFormat _ = "%sRequired"
    descFormat _ = "Поле «%s» обязательно"


data CodeTitle
instance VinFieldParameter CodeTitle where
    type ParamType CodeTitle = Maybe Text
    nameFormat _ = "%sCodeTitle"
    descFormat _ = "Заголовок кода дилера для поля «%s»"


data Title
instance VinFieldParameter Title where
    type ParamType Title = Text
    nameFormat _ = "%sTitle"
    descFormat _ = "Заголовок поля «%s»"


data Titles
instance VinFieldParameter Titles where
    type ParamType Titles = Vector Text
    nameFormat _ = "%sTitles"
    descFormat _ = "Заголовки поля «%s»"


data DateFormat
instance VinFieldParameter DateFormat where
    type ParamType DateFormat = Maybe Text
    nameFormat _ = "%sFormat"
    descFormat _ = "Формат даты для поля «%s»"


data Default t
instance Typeable t => VinFieldParameter (Default t) where
    type ParamType (Default t) = t
    nameFormat _ = "%sDefault"
    descFormat _ = "Значение по умолчанию для поля «%s»"

type family ExtraAccs (a :: FormatFieldType)

type instance ExtraAccs a = ()


-- | Semantic annotations for 'Contract' fields.
--
-- TODO Regexps for Raw fields?
data FormatFieldType = Raw
                     -- ^ Store as is.
                     | Number
                     -- ^ Store as an integer after stripping
                     -- non-digits.
                     | Phone
                     -- ^ Store as text after stripping non-digits and
                     -- leading plus sign.
                     | Date
                     | Name
                     -- ^ Concatenate several columns and store as is.
                     | Dict
                     | Dealer
                     | Subprogram


$(genSingletons [''FormatFieldType])


-- | Annotated field of the 'Contract' model.
data FF where
    FF :: (Typeable t, GHC.SingI n, GHC.SingI d) =>
          FormatFieldType -> (Contract -> F t n d) -> FF


-- | Generate VinFormat model. Haskell field accessor names equal
-- field names. Also bind @vinFormatAccessors@ to a list of
-- 'VFAccessor' values, one for every group of produced fields.
mkVinFormat :: [FF]
            -- ^ Annotated subset of 'Contract' fields.
            -> Q [Dec]
mkVinFormat formatFields =
    let
        -- Type & constructor name
        typeName = mkName "VinFormat"

        -- Primary key and label
        idnt = [ varStrictType (mkName "ident") $
                 ns $
                 appT (appT [t|PK Int|] (conT typeName)) [t|"Формат VIN"|]
               , varStrictType (mkName "label") $
                 ns $
                 [t|F Text "label" "Название формата"|]
               ]

        -- Use specified fields of 'Contract' model in constructor.
        -- Collect accessors that were produced from every source
        -- field.
        fields :: [(FF, (ExpQ, [VarStrictTypeQ]))]
        fields =
            map
            (\ff@(FF fft (proj :: Contract -> F t n d)) ->
             let
                 acc = CF proj
                 loadName     = name (undefined :: Load)     acc
                 requiredName = name (undefined :: Required) acc
             in
               (,) ff $ (,)
             [e|
              VFAcc
              $(appE [e|Sing|] 
              $(appE [e|CF|] (varE $ mkName $ T.unpack $ fieldName proj))
              $(appE [e|LoadAcc|] $ varE $ mkName loadName)
              $(appE [e|RequiredAcc|] $ varE $ mkName requiredName)
              |] $
             -- fLoad,fRequired,fTitle(s) fields
             [ mkAcc (undefined :: Load)     acc
             , mkAcc (undefined :: Required) acc
             ]
             ++
             case fft of
               Name -> [mkAcc (undefined :: Titles)  acc]
               _    -> [mkAcc (undefined :: Title)       acc]
             ++
             -- Extra fCodeTitle field for Dealer fields, extra
             -- fFormat field for Date fields
             case fft of
               Dealer -> [mkAcc (undefined :: CodeTitle)  acc]
               Date   -> [mkAcc (undefined :: DateFormat) acc]
               _      -> []
             ++
             -- Default field except for Subprogram
             case fft of
               Subprogram -> []
               _          -> [mkAcc (undefined :: Default t) acc]
            )
            $
            formatFields

        constructor = [recC typeName $ idnt ++ (concat $ map (snd . snd) fields)]

        vfas = map (fst . snd) fields
    in do
      d  <- dataD (cxt []) typeName [] constructor [''Typeable]
      d' <- [d|
             vinFormatAccessors :: [VFAccessor $(conT typeName)]
             vinFormatAccessors = $(listE vfas)|]
      return $ [d] ++ d'


-- | Base VinFormat accessors produced from a single source 'Contract'
-- field, common to all field types. Tagged with VinFormat type to
-- properly instantiate accessor types.
data VFAccessor m where
    VFAcc :: { tag      :: (SFormatFieldType a)
             , proj     :: ContractField
             -- ^ Original field @f@.
             , load     :: ParamAcc Load m
             -- ^ @fLoad@ accessor.
             , required :: ParamAcc Required m
             -- ^ @fRequired@ accessor.
--             , extras   :: (ExtraAccs a)
             } -> VFAccessor m
