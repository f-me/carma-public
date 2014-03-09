{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

VinFormat macros and meta helpers.

The idea is to list a set 'Contract' fields annotated by
'FormatFieldType' and generate VinFormat model from it. A source field
@f@ of 'Contract' is spliced into several fields (format parameters
for that source field) @fLoad@, @fRequired@ etc. of 'VinFormat',
depending on the annotation given for the source field.
'FormatFieldAccessor' type preserves information about how a source
field has is mapped to a group of produced VinFormat accessors.

-}

module Carma.Model.VinFormat.Meta
    ( FormatFieldType(..), Sing(..)
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


-- | This class describes how a format parameter is generated from a
-- source 'Contract' field.
--
-- Minimal definition includes 'nameFormat' and 'descFormat'.
class Typeable (ParamType a) => FormatFieldParameter a where
    -- | Type of parameter value (if a parameter is a checkbox, this
    -- is 'Bool' (which is also the default instance for this family).
    type ParamType a
    -- | Reified type of parameter value. This exists because it's not
    -- possible to statically encode value type to parameter
    -- annotation. This matches 'ParamType' by default, though.
    paramTypeRep :: a -> ContractField -> TypeRep

    -- | 'printf'-compatible format used to produce a parameter name
    -- from a source field name (first @%s@ is replaced by the source
    -- field name).
    nameFormat :: a -> String
    -- | The same as 'nameFormat', but for field description.
    descFormat :: a -> String

    paramTypeRep _ _ = typeOf (undefined :: ParamType a)
    type ParamType a = Bool


data Load = Load
instance FormatFieldParameter Load where
    nameFormat _ = "%sLoad"
    descFormat _ = "Загружать поле «%s»"


data Required = Required
instance FormatFieldParameter Required where
    nameFormat _ = "%sRequired"
    descFormat _ = "Поле «%s» обязательно"


data Default = Default
instance FormatFieldParameter Default where
    paramTypeRep _ (FA f) = fieldType f
    nameFormat _ = "%sDefault"
    descFormat _ = "Значение поля «%s» по умолчанию"


-- | VIN format field title type.
data FormatFieldTitle = Title
                      | MultiTitles

$(genSingletons [''FormatFieldTitle])

type SFFL a = SFormatFieldTitle a

instance FormatFieldParameter (SFFL Title) where
    type ParamType (SFFL Title) = Text
    nameFormat _ = "%sTitle"
    descFormat _ = "Заголовок поля «%s»"

instance FormatFieldParameter (SFFL MultiTitles) where
    type ParamType (SFFL MultiTitles) = Vector Text
    nameFormat _ = "%sTitles"
    descFormat _ = "Заголовки поля «%s»"


name :: FormatFieldParameter a => a -> ContractField -> String
name a cf = fieldProjFormatter (\(FA f) -> T.unpack $ fieldName f) cf $
            nameFormat a


desc :: FormatFieldParameter a => a -> ContractField -> String
desc a cf = fieldProjFormatter (\(FA f) -> T.unpack $ fieldDesc f) cf $
            descFormat a


-- | Produce a parameter accessor.
mkAcc :: FormatFieldParameter a =>
         a
      -- ^ Parameter type.
      -> ContractField
      -- ^ Source field.
      -> VarStrictTypeQ
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


-- | Semantic annotations for 'Contract' fields. This datatype defines
-- how a source field should be spliced into its format parameters and
-- how they should be used later.
--
-- Pattern-matching on these may be used to both define field
-- processing behavour in a VIN processing tool and refine some of
-- extra parameter types.
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
                     | Year
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


-- | Bind format field types and their extra parameters. Extra
-- parameters vary between different 'FormatFieldTypes'.
--
-- TODO Encode all extra parameters in a single type-level container
-- instead of using an associated type family for every parameter.
-- This will likely require proper type-level map in order to be
-- useful. This is not crucial now as we now have only one variable
-- extra parameter (title).
class (FormatFieldParameter (TitleParameter a)) => FFTypeI a where
    type TitleParameter a
    type TitleParameter a = (SFFL Title)

instance FFTypeI (SFFT Raw)

instance FFTypeI (SFFT Number)

instance FFTypeI (SFFT VIN)

instance FFTypeI (SFFT Email)

instance FFTypeI (SFFT Plate)

instance FFTypeI (SFFT Year)

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


-- | Generate VinFormat model, bind @vinFormatAccessors@ to a list of
-- 'FormatFieldAccessor' values, one for every group of fields
-- produced from a single 'Contract' field.
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
              -- We used type-level information about the annotation
              -- to generate parameters, while also preserving the
              -- annotation in 'FormatFieldAccessor' so that it can be
              -- used later.
              $(sigE [e|sing|] $
                appT [t|SFFT|] $
                conT $ mkName $ showConstr $ toConstr fallenFft)
              $(varE $ mkName (name Load acc))
              $(varE $ mkName (name Required acc))
              $(varE $ mkName (name Default acc))
              $(varE $ mkName (name titleSing acc))
              |] $
              -- fLoad,fRequired,fTitle(s) fields
              [ mkAcc Load acc
              , mkAcc Required acc
              , mkAcc Default acc
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
    (FieldI (ParamType Load) n1 d1,
     FieldI (ParamType Required) n2 d2,
     ToField t, Typeable t, FieldI t n3 d3,
     FieldI (ParamType (TitleParameter (SFFT a))) n4 d4) =>
    FFAcc { cf       :: ContractField
          -- ^ Original field @f@.
          , tag      :: SFFT a
          -- ^ Original annotation. Pattern matching on this field
          -- will refine types of extra parameters.
          , load     :: (m -> F (ParamType Load) n1 d1)
          -- ^ @fLoad@ accessor.
          , required :: (m -> F (ParamType Required) n2 d2)
          -- ^ @fRequired@ accessor.
          , def      :: (m -> F t n3 d3)
          -- ^ @fDefault@ accessor.
          , title    :: (m -> F (ParamType (TitleParameter (SFFT a))) n4 d4)
          }
