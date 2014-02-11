{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

VinFormat macros and meta helpers.

The idea is to list a set of annotated 'Contract' fields and generate
VinFormat model from it. Each source field @f@ is spliced into several
fields @fLoad@, @fRequired@ etc., depending on the annotation given
for that field. 'FormatFieldAccessor' type preserves information about
how a source field has is mapped to a group of produced VinFormat
accessors.

-}

module Carma.Model.VinFormat.Meta
    ( FormatFieldType(..), FormatFieldTitle(..), Sing(..)
    , ContractField, FF(..)
    , mkVinFormat
    , FormatFieldAccessor(..)
    )

where

import Data.Data
import Data.Singletons.List
import Data.Singletons.TH
import Language.Haskell.TH hiding (Name)


import Data.Text as T (Text, unpack)
import Text.Printf
import Data.Vector (Vector)
import Database.PostgreSQL.Simple.ToField

import Data.Model
import Data.Model.TH (typeRepToType)
import Data.Model.Types

import Carma.Model.Contract (Contract)


-- | Existential wrapper for 'Contract' field accessors.
type ContractField = FA Contract


fieldProjFormatter :: (ContractField -> String)
                   -> ContractField
                   -> String
                   -- ^ 'printf' format.
                   -> String
fieldProjFormatter proj cf format = printf format $ proj cf


ns :: TypeQ -> StrictTypeQ
ns = strictType (return NotStrict)


-- | VIN format field parameter.
data FormatFieldParameter = Load
                          | Required
                          | Format
                          | Default


data FormatFieldTitle = Title
                      | MultiTitles


$(genSingletons [''FormatFieldParameter, ''FormatFieldTitle])


type SFFP a = SFormatFieldParameter a
type SFFL a = SFormatFieldTitle a


-- | Minimal definition includes 'nameFormat' and 'descFormat'.
class Typeable (ParamType a) => FFParameterI a where
    paramTypeRep :: a -> ContractField -> TypeRep
    type ParamType a

    nameFormat :: a -> String
    descFormat :: a -> String

    paramTypeRep _ _ = typeOf (undefined :: ParamType a)
    type ParamType a = Bool

instance FFParameterI (SFFP Load) where
    nameFormat _ = "%sLoad"
    descFormat _ = "Загружать поле «%s»"

instance FFParameterI (SFFP Required) where
    nameFormat _ = "%sRequired"
    descFormat _ = "Поле «%s» обязательно"

instance FFParameterI (SFFL Title) where
    type ParamType (SFFL Title) = Text
    nameFormat _ = "%sTitle"
    descFormat _ = "Заголовок поля «%s»"

instance FFParameterI (SFFL MultiTitles) where
    type ParamType (SFFL MultiTitles) = Vector Text
    nameFormat _ = "%sTitles"
    descFormat _ = "Заголовки поля «%s»"

instance FFParameterI (SFFP Format) where
    type ParamType (SFFP Format) = Text
    nameFormat _ = "%sFormat"
    descFormat _ = "Формат для поля «%s»"

instance FFParameterI (SFFP Default) where
    paramTypeRep _ (FA f) = fieldType f
    nameFormat _ = "%sDefault"
    descFormat _ = "Значение поля «%s» по умолчанию"


name :: FFParameterI a => a -> ContractField -> String
name a cf = fieldProjFormatter (\(FA f) -> T.unpack $ fieldName f) cf $
            nameFormat a


desc :: FFParameterI a => a -> ContractField -> String
desc a cf = fieldProjFormatter (\(FA f) -> T.unpack $ fieldDesc f) cf $
            descFormat a


mkAcc :: FFParameterI a => a -> ContractField -> VarStrictTypeQ
mkAcc a cf =
    varStrictType (mkName n) $ ns $
    [t|
     F $(return $ typeRepToType t)
     $(litT $ strTyLit n)
     $(litT $ strTyLit d)|]
    where
      n = name a cf
      d = desc a cf
      t = paramTypeRep a cf


-- | Semantic annotations for 'Contract' fields.
--
-- TODO Regexps for Raw fields?
data FormatFieldType = Raw
                     -- ^ Store as is.
                     | Number
                     -- ^ Store as an integer after stripping
                     -- non-digits.
                     | VIN
                     | Email
                     | Plate
                     | Phone
                     -- ^ Store as text after stripping non-digits and
                     -- leading plus sign.
                     | Name
                     -- ^ Concatenate several columns and store as is.
                     | Date
                     | Dict
                     | Dealer
                     | Subprogram
                       deriving (Data, Typeable)

$(genSingletons [''FormatFieldType])


type SFFT a = SFormatFieldType a


-- Bind format field types and their extra parameters.
class (FFParameterI (TitleParameter a)) => FFTypeI a where
    type TitleParameter a
    type TitleParameter a = (SFFL Title)

instance FFTypeI (SFFT Raw)

instance FFTypeI (SFFT Number)

instance FFTypeI (SFFT VIN)

instance FFTypeI (SFFT Email)

instance FFTypeI (SFFT Plate)

instance FFTypeI (SFFT Phone)

instance FFTypeI (SFFT Date)

instance FFTypeI (SFFT Name) where
    type TitleParameter (SFFT Name) = (SFFL MultiTitles)

instance FFTypeI (SFFT Dict)

instance FFTypeI (SFFT Dealer) where
    type TitleParameter (SFFT Dealer) = (SFFL MultiTitles)

instance FFTypeI (SFFT Subprogram)


-- | Annotated field of the 'Contract' model.
data FF where
    FF :: forall a v t n d.
          (FFTypeI (SFFT a), TitleParameter (SFFT a) ~ SFFL v, SingI v,
           ToField t, FieldI t n d) =>
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
        fields :: [(ExpQ, [VarStrictTypeQ])]
        fields =
            map
            (\(FF (fft :: SFFT a) proj) ->
             let
                 acc = FA proj
                 -- Lack of Data instance for singletons means that we
                 -- need to reconstruct singleton from the name of
                 -- demoted value constructor.
                 fallenFft :: FormatFieldType
                 fallenFft = fromSing fft
                 titleSing = sing :: TitleParameter (SFFT a)
             in
               (,)
             [e|
              FFAcc
              $(appE [e|FA|] (varE $ mkName $ T.unpack $ fieldName proj))
              $(sigE [e|sing|] $
                appT [t|SFFT|] $ conT $ mkName $ showConstr $ toConstr fallenFft)
              $(varE $ mkName (name SLoad acc))
              $(varE $ mkName (name SRequired acc))
              $(varE $ mkName (name SDefault acc))
              $(varE $ mkName (name titleSing acc))
              |] $
             -- fLoad,fRequired,fTitle(s) fields
             [ mkAcc SLoad acc
             , mkAcc SRequired acc
             , mkAcc SDefault acc
             , mkAcc titleSing acc
             ]
            )
            $
            formatFields

        constructor = [recC typeName $
                       basic ++
                       (concat $ map snd fields)
                      ]

        vfas = map fst fields
    in do
      d  <- dataD (cxt []) typeName [] constructor [''Typeable]
      d' <- [d|
             vinFormatAccessors :: [FormatFieldAccessor $(conT typeName)]
             vinFormatAccessors = $(listE vfas)|]
      return $ [d] ++ d'


-- | A group of VinFormat accessors produced from a single source
-- 'Contract' field; existential container for 'FormatFieldType'
-- singleton value to enable type refinement of accessor types.
data FormatFieldAccessor m =
    forall a n1 d1 n2 d2 t n3 d3 n4 d4.
    (FieldI (ParamType (SFFP Load)) n1 d1,
     FieldI (ParamType (SFFP Required)) n2 d2,
     ToField t, Typeable t, FieldI t n3 d3,
     FieldI (ParamType (TitleParameter (SFFT a))) n4 d4) =>
    FFAcc { cf       :: ContractField
          -- ^ Original field @f@.
          , tag      :: SFFT a
          -- ^ Original annotation. Pattern matching on this field
          -- will refine types of extra parameters.
          , load     :: (m -> F (ParamType (SFFP Load)) n1 d1)
          -- ^ @fLoad@ accessor.
          , required :: (m -> F (ParamType (SFFP Required)) n2 d2)
          -- ^ @fRequired@ accessor.
          , def      :: (m -> F t n3 d3)
          -- ^ @fDefault@ accessor.
          , title    :: (m -> F (ParamType (TitleParameter (SFFT a))) n4 d4)
          }
