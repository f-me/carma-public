{-|

VinFormat macros and meta helpers.

The idea is to list a set of annotated 'Contract' fields and generate
VinFormat model from it. Each source field @f@ is spliced into several
fields @fLoad@, @fRequired@ etc., depending on the annotation given
for that field. 'VFAccessor' type preserves information about how a
source field has is mapped to a group of produced VinFormat accessors.

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat.Meta
    ( FormatFieldType(..), Sing(..), SFFT(..)
    , ContractField(..), FF(..)
    , mkVinFormat
    , VFAccessor(..), FAccessor(..)
    )

where

import Data.Data
import Data.Typeable
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
--
-- Minimal definition includes 'nameFormat' and 'descFormat'.
data FormatFieldParameter =
    FFP { paramType  :: ContractField -> TypeRep
        , nameFormat :: String
        , descFormat :: String
        }

name a cf = fieldProjFormatter (\(CF f) -> T.unpack $ fieldName f) cf $
            nameFormat a

desc a cf = fieldProjFormatter (\(CF f) -> T.unpack $ fieldDesc f) cf $
            descFormat a

mkAcc :: FormatFieldParameter -> ContractField -> VarStrictTypeQ
mkAcc a cf =
    varStrictType (mkName n) $ ns $
    [t|
     F $(return $ typeRepToType t)
     $(litT $ strTyLit n)
     $(litT $ strTyLit d)|]
    where
      n = name a cf
      d = desc a cf
      t = paramType a cf


constTypeOf :: Typeable a => a -> (ContractField -> TypeRep)
constTypeOf v = const $ typeOf v


loadParameter =
    FFP (constTypeOf (undefined :: Bool)) "%sLoad" "Загружать поле «%s»"
requiredParameter =
    FFP (constTypeOf (undefined :: Bool)) "%sRequired" "Поле «%s» обязательно"
codeTitleParameter =
    FFP (constTypeOf (undefined :: Maybe Text)) "%sCodeTitle" "Заголовок кода дилера для поля «%s»"
titleParameter =
    FFP (constTypeOf (undefined :: Text)) "%sTitle" "Заголовок поля «%s»"
titlesParameter =
    FFP (constTypeOf (undefined :: Vector Text)) "%sTitles" "Заголовки поля «%s»"
formatParameter =
    FFP (constTypeOf (undefined :: Text)) "%sFormat" "Формат для поля «%s»"


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
class FFTypeI a where
    type TitleParameter a
    type ExtraParameters a
    titlePar :: a -> FormatFieldParameter
    extraPars :: a -> [FormatFieldParameter]

    type TitleParameter a = Text
    type ExtraParameters a = ()
    titlePar _ = titleParameter
    extraPars _ = []


instance FFTypeI (SFFT Raw)

instance FFTypeI (SFFT Number)

instance FFTypeI (SFFT Phone)

instance FFTypeI (SFFT Date)

instance FFTypeI (SFFT Name) where
    type TitleParameter (SFFT Name) = Vector Text
    titlePar _ = titlesParameter

instance FFTypeI (SFFT Dict)

instance FFTypeI (SFFT Dealer)

instance FFTypeI (SFFT Subprogram)


-- | Annotated field of the 'Contract' model.
data FF where
    FF :: forall a t n d.
          (FFTypeI (SFFT a),
           Typeable t, GHC.SingI n, GHC.SingI d) =>
          (SFFT a) -> (Contract -> F t n d) -> FF


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
              VFAcc
              $(appE [e|CF|] (varE $ mkName $ T.unpack $ fieldName proj))
              $(sigE [e|sing|] $
                appT [t|SFFT|] $ conT $ mkName $ showConstr $ toConstr fallenFft)
              $(appE [e|FAcc|] (varE $ mkName (name loadParameter acc)))
              $(appE [e|FAcc|] (varE $ mkName (name requiredParameter acc)))
              $(appE [e|FAcc|] (varE $ mkName (name (titlePar fft) acc)))
              |] $
             -- fLoad,fRequired,fTitle(s) fields
             [ mkAcc loadParameter acc
             , mkAcc requiredParameter  acc
             , mkAcc (titlePar fft) acc
             ]
            )
            $
            formatFields

        constructor = [recC typeName $ basic ++ (concat $ map (snd . snd) fields)]

        vfas = map (fst . snd) fields
    in do
      d  <- dataD (cxt []) typeName [] constructor [''Typeable]
      d' <- [d|
             vinFormatAccessors :: [VFAccessor $(conT typeName)]
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
          , title    :: FAccessor m (TitleParameter (SFFT a))
          }
