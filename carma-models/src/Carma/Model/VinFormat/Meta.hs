{-|

VinFormat macros and meta helpers.

The idea is to list a set of annotated 'Contract' fields and generate
VinFormat model from it. Each source field @f@ is spliced into several
fields @fLoad@, @fRequired@ etc., depending on the annotation given
for that field. A source field is mapped to a group of produced
VinFormat accessors are available using 'VFAccessor' type.

-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat.Meta
    ( FF(..)
    , FormatFieldType(..)
    , mkVinFormat
    , VFAccessor(..)
    )

where

import Data.Typeable
import GHC.TypeLits
import Language.Haskell.TH

import Data.Text as T (Text, unpack)
import Text.Printf
import Data.Vector (Vector)

import Data.Model

import Carma.Model.Contract (Contract)


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


-- | Annotated field of the 'Contract' model.
data FF = forall t n d. (Typeable t, SingI n, SingI d) =>
          FF FormatFieldType (Contract -> F t n d)


-- | Convert 'TypeRep' to Template Haskell 'Type'.
typeRepToType :: TypeRep -> Type
typeRepToType tr =
    let
        tyConToType tyCon = ConT $ mkName $ tyConName tyCon
        (conTr, trs)      = splitTyConApp tr
    in
      foldl (\t m -> AppT t (typeRepToType m)) (tyConToType conTr) trs


-- | Generate VinFormat model. Haskell field accessor names equal
-- field names. Also bind @vinFormatAccessors@ to a list of
-- 'VFAccessor' values, one for every group of produced fields.
mkVinFormat :: [FF]
            -- ^ Annotated subset of 'Contract' fields.
            -> Q [Dec]
mkVinFormat formatFields =
    let
        -- Type & constructor name
        name = mkName "VinFormat"

        loadSuffix = "Load"
        requiredSuffix = "Required"
        titleSuffix (FF fft _) =
            case fft of
              Name -> "Titles"
              _    -> "Title"
        codeSuffix = "CodeTitle"
        formatSuffix = "Format"
        defaultSuffix = "Default"

        ns = strictType (return NotStrict)

        -- Primary key and label
        idnt = [ varStrictType (mkName "ident") $
                 ns $
                 appT (appT [t|PK Int|] (conT name)) [t|"Формат VIN"|]
               , varStrictType (mkName "label") $
                 ns $
                 [t|F Text "label" "Название формата"|]
               ]

        fnWithSuffix (FF _ proj) suf =
            T.unpack (fieldName proj) ++ suf

        fdFormatted (FF _ proj) format =
            printf format $ T.unpack $ fieldDesc proj

        -- Include specified fields of 'Contract' model in
        -- constructor. Collect names of fields that were produced
        -- from every source field.
        fields :: [(FF, String, String, [VarStrictTypeQ])]
        fields =
            map
            (\ff@(FF fft proj) ->
             let
                 loadName     = fnWithSuffix ff loadSuffix
                 requiredName = fnWithSuffix ff requiredSuffix
                 titleName    = fnWithSuffix ff $ titleSuffix ff
                 codeName     = fnWithSuffix ff codeSuffix
                 formatName   = fnWithSuffix ff formatSuffix
                 defaultName  = fnWithSuffix ff defaultSuffix
             in
               (,,,) ff loadName requiredName $
             -- fLoad,fRequired,fTitle(s) fields
             [ varStrictType (mkName loadName) $
                 ns $
                 [t|
                  F Bool
                  $(litT $ strTyLit loadName)
                  $(litT $ strTyLit $ fdFormatted ff "Загружать поле «%s»")|]
             , varStrictType (mkName requiredName) $
                 ns $
                 [t|
                  F Bool
                  $(litT $ strTyLit requiredName)
                  $(litT $ strTyLit $
                    fdFormatted ff "Поле «%s» обязательно")|]
             , varStrictType (mkName titleName) $
                 ns $
                 [t|
                  F $(case fft of
                        Name -> [t|Vector Text|]
                        _    -> [t|Text|])
                  $(litT $ strTyLit titleName)
                  $(litT $ strTyLit $ fdFormatted ff "Заголовок поля «%s»")|]
             ]
             ++
             -- Extra fCodeTitle field for Dealer fields
             case fft of
               Dealer ->
                   [ varStrictType (mkName codeName) $
                     ns $
                     [t|
                      F Text
                      $(litT $ strTyLit codeName)
                      $(litT $ strTyLit $ fdFormatted ff
                        "Заголовок кода дилера для поля «%s»")|]
                     ]
               _ -> []
             ++
             -- Extra fFormat field for Date fields
             case fft of
               Date ->
                   [ varStrictType (mkName formatName) $
                     ns $
                     [t|
                      F (Maybe Text)
                      $(litT $ strTyLit formatName)
                      $(litT $ strTyLit $ fdFormatted ff
                        "Формат даты для поля «%s»")|]
                   ]
               _ -> []
             ++
             -- Default field except for Subprogram
             case fft of
               Subprogram -> []
               _ ->
                   [ varStrictType (mkName defaultName) $
                     ns $
                     [t|
                      F $(return $ typeRepToType $ fieldType proj)
                      $(litT $ strTyLit defaultName)
                      $(litT $ strTyLit $
                        fdFormatted ff "Значение поля «%s» по умолчанию")|]
                   ]
            )
            $
            formatFields
        constructor = [recC name $
                       idnt ++ (concat $ map (\(_, _, _, vs) -> vs) fields)
                      ]
        vfAccs = map
                 (\((FF fft proj), l, r, _) ->
                      [e|
                       VFAcc
                       $(varE $ mkName $ T.unpack $ fieldName proj)
                       $(varE $ mkName l)
                       $(varE $ mkName r)
                       |])
                 fields
    in do
      d  <- dataD (cxt []) name [] constructor [''Typeable]
      d' <- [d|
             vinFormatAccessors :: [VFAccessor $(conT name)]
             vinFormatAccessors = $(listE vfAccs)
             |]
      return $ [d] ++ d'


-- | A set of VinFormat accessors produced from a single source
-- 'Contract' field.
data VFAccessor m =
    forall n1 n2 d1 d2 t n d.
    (Typeable t, SingI n, SingI d,
     SingI n1, SingI d1,
     SingI n2, SingI d2) =>
    VFAcc { proj     :: Contract -> F t n d
          -- ^ Original field @f@.
          , load     :: m -> F Bool n1 d1
          -- ^ @fLoad@ accessor.
          , required :: m -> F Bool n2 d2
          -- ^ @fRequired@ accessor.
          }
