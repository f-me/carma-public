{-|

Macro generating VinFormat model from Contract.

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


-- | Semantic annotations for 'Contract' fields which may be used when
-- processing external data.
--
-- TODO Regexps for Raw fields?
data FormatFieldType = Raw
                     | Date
                     | Dict
                     | Dealer
                     | Number
                     | Phone
                     | Name
                     | Subprogram
                     | Skip


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

        -- Primary key field
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
        -- constructor.
        fields =
            concat $
            map (\ff@(FF fft proj) ->
                 -- *{Load,Required,Title(s)} fields
                 [ varStrictType (mkName $ fnWithSuffix ff loadSuffix) $
                   ns $
                   [t|
                    F Bool
                    $(litT $ strTyLit $ fnWithSuffix ff loadSuffix)
                    $(litT $ strTyLit $ fdFormatted ff "Загружать поле «%s»")|]
                 , varStrictType (mkName $ fnWithSuffix ff requiredSuffix) $
                   ns $
                   [t|
                    F Bool
                    $(litT $ strTyLit $ fnWithSuffix ff requiredSuffix)
                    $(litT $ strTyLit $
                      fdFormatted ff "Поле «%s» обязательно")|]
                 , varStrictType (mkName $ fnWithSuffix ff $ titleSuffix ff) $
                   ns $
                   [t|
                    F $(case fft of
                          Name -> [t|Vector Text|]
                          _    -> [t|Text|])
                    $(litT $ strTyLit $ fnWithSuffix ff $ titleSuffix ff)
                    $(litT $ strTyLit $ fdFormatted ff "Заголовок поля «%s»")|]
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
        constructor = [recC name $ idnt ++ fields]
    in do
      d <- dataD (cxt []) name [] constructor [''Typeable]
      return [d]
