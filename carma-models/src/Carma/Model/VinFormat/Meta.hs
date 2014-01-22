{-|

VinFormat macros and meta helpers.

The idea is to list a set of annotated 'Contract' fields and generate
VinFormat model from it. Each source field @f@ is spliced into several
fields @fLoad@, @fRequired@ etc., depending on the annotation given
for that field. 'FormatFieldAccessor' type preserves information about
how a source field has is mapped to a group of produced VinFormat
accessors.

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat.Meta
    ( FormatFieldType(..), Sing(..)
    , ContractField(..), FF(..)
    , mkVinFormat
    , FormatFieldAccessor(..), FAccessor(..)
    )

where

import Data.Data
import Data.Singletons.List
import Data.Singletons.TH
import qualified GHC.TypeLits as GHC
import Language.Haskell.TH hiding (Name)


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
data FormatFieldParameter = Load
                          | Required
                          | CodeTitle
                          | Title
                          | MultiTitles
                          | Format

$(genSingletons [''FormatFieldParameter])

type SFFP a = SFormatFieldParameter a


-- | Minimal definition includes 'nameFormat' and 'descFormat'.
class Typeable (ParamType a) => FFParameterI a where
    type ParamType a
    nameFormat :: a -> String
    descFormat :: a -> String

    type ParamType a = Bool

instance FFParameterI (SFFP Load) where
    nameFormat _ = "%sLoad"
    descFormat _ = "Загружать поле «%s»"

instance FFParameterI (SFFP Required) where
    nameFormat _ = "%sRequired"
    descFormat _ = "Поле «%s» обязательно"

instance FFParameterI (SFFP CodeTitle) where
    type ParamType (SFFP CodeTitle) = Maybe Text
    nameFormat _ = "%sCodeTitle" 
    descFormat _ = "Заголовок кода дилера для поля «%s»"

instance FFParameterI (SFFP Title) where
    type ParamType (SFFP Title) = Text
    nameFormat _ = "%sTitle"
    descFormat _ = "Заголовок поля «%s»"

instance FFParameterI (SFFP MultiTitles) where
    type ParamType (SFFP MultiTitles) = Vector Text
    nameFormat _ = "%sTitles"
    descFormat _ = "Заголовки поля «%s»"

instance FFParameterI (SFFP Format) where
    type ParamType (SFFP Format) = Text
    nameFormat _ = "%sFormat"
    descFormat _ = "Формат для поля «%s»"


name :: FFParameterI a => a -> ContractField -> String
name a cf = fieldProjFormatter (\(CF f) -> T.unpack $ fieldName f) cf $
            nameFormat a

desc :: FFParameterI a => a -> ContractField -> String
desc a cf = fieldProjFormatter (\(CF f) -> T.unpack $ fieldDesc f) cf $
            descFormat a

mkAcc :: forall a.
         FFParameterI a => a -> ContractField -> VarStrictTypeQ
mkAcc a cf =
    varStrictType (mkName n) $ ns $
    [t|
     F $(return $ typeRepToType t)
     $(litT $ strTyLit n)
     $(litT $ strTyLit d)|]
    where
      n = name a cf
      d = desc a cf
      t = typeOf (undefined :: ParamType a)


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
                       deriving (Data, Typeable)

$(genSingletons [''FormatFieldType])

type SFFT a = SFormatFieldType a


-- Bind format field types and their extra parameters.
class (FFParameterI (TitleParameter a)) => FFTypeI a where
    type TitleParameter a
    type TitleParameter a = (SFFP Title)


instance FFTypeI (SFFT Raw)

instance FFTypeI (SFFT Number)

instance FFTypeI (SFFT Phone)

instance FFTypeI (SFFT Date)

instance FFTypeI (SFFT Name) where
    type TitleParameter (SFFT Name) = (SFFP MultiTitles)

instance FFTypeI (SFFT Dict)

instance FFTypeI (SFFT Dealer)

instance FFTypeI (SFFT Subprogram)


titlePar :: forall a. FFTypeI a => a -> TitleParameter a
titlePar = undefined


-- | Annotated field of the 'Contract' model.
data FF where
    FF :: forall a t n d.
          (FFTypeI (SFFT a),
           Typeable t, GHC.SingI n, GHC.SingI d) =>
          (SFFT a) -> (Contract -> F t n d) -> FF


-- | Generate VinFormat model. Haskell field accessor names equal
-- field names. Also bind @vinFormatAccessors@ to a list of
-- 'FormatFieldAccessor' values, one for every group of produced
-- fields.
mkVinFormat :: [FF]
            -- ^ Annotated subset of 'Contract' fields.
            -> Q [Dec]
mkVinFormat formatFields =
    let
        -- Type & constructor name
        typeName = mkName "VinFormat"

        -- Primary key and label
        basic = [ varStrictType (mkName "ident") $
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
            (\ff@(FF fft proj) ->
             let
                 acc = CF proj
                 -- Lack of Data instance for singletons means that we
                 -- need to reconstruct singleton from the name of
                 -- demoted value constructor.
                 fallenFft :: FormatFieldType
                 fallenFft = fromSing fft
             in
               (,) ff $ (,)
             [e|
              FFAcc
              $(appE [e|CF|] (varE $ mkName $ T.unpack $ fieldName proj))
              $(sigE [e|sing|] $
                appT [t|SFFT|] $ conT $ mkName $ showConstr $ toConstr fallenFft)
              $(appE [e|FAcc|] (varE $ mkName (name SLoad acc)))
              $(appE [e|FAcc|] (varE $ mkName (name SRequired acc)))
              $(appE [e|FAcc|] (varE $ mkName (name (titlePar fft) acc)))
              |] $
             -- fLoad,fRequired,fTitle(s) fields
             [ mkAcc SLoad acc
             , mkAcc SRequired  acc
             , mkAcc (titlePar fft) acc
             ]
            )
            $
            formatFields

        constructor = [recC typeName $
                       basic ++
                       (concat $ map (snd . snd) fields)
                      ]

        vfas = map (fst . snd) fields
    in do
      d  <- dataD (cxt []) typeName [] constructor [''Typeable]
      d' <- [d|
             vinFormatAccessors :: [FormatFieldAccessor $(conT typeName)]
             vinFormatAccessors = $(listE vfas)|]
      return $ [d] ++ d'


data FAccessor m t = forall n d. FAcc (m -> F t n d)


-- | Base VinFormat accessors produced from a single source 'Contract'
-- field, common to all field types.
data VFAccessor m =
    forall a.
    VFAcc { proj     :: ContractField
          , tag      :: SFFT a
          -- ^ Original field @f@.
          , load     :: FAccessor m Bool
          -- ^ @fLoad@ accessor.
          , required :: FAccessor m Bool
          , title    :: FAccessor m (ParamType (TitleParameter (SFFT a)))
          }
