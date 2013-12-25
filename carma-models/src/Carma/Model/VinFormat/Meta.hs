{-|

VinFormat macros and meta helpers.

The idea is to list a set of annotated 'Contract' fields and generate
VinFormat model from it. Each source field @f@ is spliced into several
fields @fLoad@, @fRequired@ etc., depending on the annotation given
for that field.

|-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.VinFormat.Meta
    ( FF(..)
    , FormatFieldType(..)
    , mkVinFormat
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
          deriving Typeable


-- | Convert 'TypeRep' to Template Haskell 'Type'.
typeRepToType :: TypeRep -> Type
typeRepToType tr =
    let
        tyConToType tyCon = ConT $ mkName $ tyConName tyCon
        (conTr, trs)      = splitTyConApp tr
    in
      foldl (\t m -> AppT t (typeRepToType m)) (tyConToType conTr) trs


-- | Generate VinFormat model. Haskell field accessor names equal
-- field names.
mkVinFormat :: String
            -- ^ VIN format type/constructor name.
            -> [FF]
            -- ^ Annotated subset of 'Contract' fields.
            -> Q [Dec]
mkVinFormat vfTypeName formatFields =
    let
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
        name = mkName vfTypeName

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
        fields :: [(FF, [VarStrictTypeQ])]
        fields =
            map 
            (\ff@(FF fft proj) ->
             let
                 loadName = fnWithSuffix ff loadSuffix
                 titleName = fnWithSuffix ff $ titleSuffix ff
                 requiredName = fnWithSuffix ff requiredSuffix
             in
               (,) ff $
             -- *{Load,Required,Title(s)} fields
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
                $(litT $ strTyLit $ fnWithSuffix ff $ titleSuffix ff)
                $(litT $ strTyLit $ titleName)|]
             ]
             ++
             -- Extra *CodeTitle field for Dealer fields
             case fft of
               Dealer ->
                   [ varStrictType
                     (mkName $ fnWithSuffix ff codeSuffix) $
                     ns $
                     [t|
                      F Text
                      $(litT $ strTyLit $ fnWithSuffix ff codeSuffix)
                      $(litT $ strTyLit $ fdFormatted ff
                        "Заголовок кода дилера для поля «%s»")|]
                   ]
               _ -> []
             ++
             -- Extra *Format field for Date fields
             case fft of
               Date ->
                   [ varStrictType
                     (mkName $ fnWithSuffix ff formatSuffix) $
                     ns $
                     [t|
                      F (Maybe Text)
                      $(litT $ strTyLit $ fnWithSuffix ff formatSuffix)
                      $(litT $ strTyLit $ fdFormatted ff
                        "Формат даты для поля «%s»")|]
                   ]
               _ -> []
             ++
             -- Default field except for Subprogram
             case fft of
               Subprogram -> []
               _ ->
                   [ varStrictType
                     (mkName $ fnWithSuffix ff defaultSuffix) $
                     ns $
                     [t|
                      F $(return $ typeRepToType $ fieldType proj)
                      $(litT $ strTyLit $ fnWithSuffix ff defaultSuffix)
                      $(litT $ strTyLit $
                        fdFormatted ff "Значение поля «%s» по умолчанию")|]
                   ]
            )
            $
            formatFields
        constructor = [recC name $ idnt ++ (concat $ map snd fields)]
    in do
      d <- dataD (cxt []) name [] constructor [''Typeable]
      return [d]


-- | A set of VinFormat accessors produced from a single source
-- 'Contract' field.
data VFMeta = forall m n1 n2 n3 d1 d2 d3 v t n d.
              Model m =>
              VFMeta { load     :: m -> F Bool n1 d1
                       -- ^ fLoad accessor.
                     , required :: m -> F Bool n2 d2
                       -- ^ fRequired accessor.
                     , fft      :: FormatFieldType
                     , proj     :: Contract -> F t n d
                     }
