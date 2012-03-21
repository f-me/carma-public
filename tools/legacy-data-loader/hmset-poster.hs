{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Post legacy data directly to Redis hashes, cases only.

TODO More row transformations.

TODO Support models other than "case".

TODO Support more than one dependant form.

|-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM, when)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.ByteString.Lazy.UTF8 as LBU (toString)
import Data.Enumerator as E hiding (foldM, map, head)
import qualified Data.Enumerator.Binary as EB

import Data.Maybe (catMaybes)
import qualified Data.Map as M hiding (map)

import Data.CSV.Enumerator as CSV

import Database.Redis

import System.Console.CmdArgs.Implicit

import Snap.Snaplet.Redson.Snapless.CRUD
import Snap.Snaplet.Redson.Snapless.Metamodel

data Options = Options
    { caseFile :: Maybe FilePath
    , index    :: [String]
    }
    deriving (Show, Data, Typeable)


-- | Process case row.
--
-- * omit empty fields
--
-- TODO: Combine dates and names
filterCaseRow :: MapRow -> MapRow
filterCaseRow row = snd $ M.mapEither 
              (\v -> if v == "" then Left v else Right v)
              row


-- | Describes how to build instance fields from CSV field names or literal
-- values.

-- Left field value is read as is while Right indicates the field of
-- original row to populate the new instance field from.
--
-- TODO allow arbitary transformation functions so that dictionaries
-- may be supported.
--
-- TODO Should be written from left to right (csv → model,
-- not model ← csv)
type FieldMap = M.Map FieldName (Either FieldValue FieldName)

-- 'FieldMap' with unfixed strings and in list form
type ProtoMap = [(String, Either String String)]

-- How to match transformation
data Match = Match { matchers :: [FieldValue]
                   -- ^ legacy data field values to match this
                   -- transformation
                     
                   , matchField :: FieldName
                   -- ^ name of field to match against
                   }
           | AlwaysMatch

-- | Describes how to build new commit for dependant instance and how it
-- should be referenced from parent instance.
data Transformation =
    Transformation { match :: Match
                   , referenceModel :: ModelName
                   -- ^ reference to what model create on this commit

                   , referenceField :: FieldName
                   -- ^ what field of case model will hold the
                   -- <service>:<id> reference

                   , commitMap :: FieldMap
                   }

-- | Map field names 1-to-1, no name fixing
bijectionMap :: [String] -> ProtoMap
bijectionMap fields = Prelude.map (\n -> (n, Right n)) fields

-- TODO Find out how to use <$> here
fixUtfMap :: ProtoMap -> FieldMap
fixUtfMap cmap = M.fromList 
                 (map (\(k, v) -> (f k, case v of
                                          Left s -> Left $ f s
                                          Right s -> Right $ f s))
                  cmap)
    where f = BU.fromString

-- | Build transformation using string literals
--
-- This will match on given field and one of values.
--
-- TODO OverloadedStrings breaks UTF-8 literals.
mkTransformField :: [String] 
                 -> String
                 -> ModelName 
                 -> FieldName
                 -> ProtoMap
                 -> Transformation
mkTransformField matchers mfield model rfield spec = 
    Transformation (Match (map f matchers) (f mfield)) model rfield $ 
                   fixUtfMap spec
        where f = BU.fromString

-- | Build transformation using string literals
--
-- This will always match.
mkTransformAlways :: ModelName 
                  -> FieldName
                  -> ProtoMap
                  -> Transformation
mkTransformAlways model rfield spec =
    Transformation AlwaysMatch model rfield $ fixUtfMap spec

serviceField = "Услуга  (Обязательное поле)"

tech = mkTransformField
       ["ВСКРЫТИЕ ЗАМКОВ", "ДОСТАВКА ТОПЛИВА", 
        "ЗАМЕНА КОЛЁС", "ПОДЗАРЯДКА АКБ",
        "ПОДМЕННЫЙ АВТОМОБИЛЬ", "СНЯТИЕ С ПАРКИНГА",
        "ТЕХНИЧЕСКАЯ ПОМОЩЬ"] 
        serviceField "tech" "services"
       [ ("techType", Right serviceField)
       , ("caseAddress", Right "Адрес места поломки")
       , ("techContractor", Right "Название партнёра")
       , ("techComments", Right "Описание неисправности со слов клиента")
       ]

hotel = mkTransformField
       ["ГОСТИНИЦА"] 
       serviceField "hotel" "services"
       [ ("caseAddress", Right "Адрес места поломки")
       ]

towageCommonMap = [ ("caseAddress", Right "Адрес места поломки")
                  , ("towDealer", Right "Название дилера куда эвакуируют автомобиль")
                  , ("towAddress", Right "Адрес куда эвакуируют автомобиль")
                  , ("towContractor", Right "Название партнёра")
                  , ("wheelsUnblocked", Left "1")
                  , ("manipulatorPossible", Left "1")
                  ]

-- TODO towerType & towType must be dictionary references!
towage1 = mkTransformField
          ["ЭВАКУАТОР", "ЭВАКУАЦИЯ В СЛУЧАЕ ДТП И ВАНДАЛИЗМА"]
          serviceField "towage" "services"
          $ towageCommonMap ++ [ ("towerType", Left "Эвакуатор")
                               , ("towType", Left "К дилеру")
                               ]

towage2 = mkTransformField
          ["ЭВАКУАЦИЯ ДО ДОМА В НОЧНОЕ ВРЕМЯ"] 
          serviceField "towage" "services"
          $ towageCommonMap ++ [ ("towerType", Left "Эвакуатор")
                               , ("towType", Left "К дому в ночное время")
                               ]

towage3 = mkTransformField
          ["ЭВАКУАЦИЯ ДО ШИНОМОНТАЖА"] 
          serviceField "towage" "services"
          $ towageCommonMap ++ [ ("towerType", Left "Эвакуатор")
                               , ("towType", Left "До мастерской")
                               ]

towage4 = mkTransformField
          ["МАНИПУЛЯТОР"]
          serviceField "towage" "services"
          $ towageCommonMap ++ [ ("towerType", Left "Манипулятор")
                               , ("towType", Left "К дилеру")
                               ]

taxi = mkTransformField
       ["ТАКСИ"]
       serviceField "taxi" "services"
       [ ("taxiFrom", Left "Адрес места поломки")
       , ("taxiTo", Left "Адрес куда эвакуируют автомобиль")
       ]

rent = mkTransformField
       ["ПОДМЕННЫЙ АВТОМОБИЛЬ"]
       serviceField "rent" "services"
       [ ("towDealer", Left "Название дилера куда эвакуируют автомобиль")
       , ("rentAddress", Left "Адрес куда эвакуируют автомобиль")
       , ("carClass", Right "")
       , ("rentContractor", Left "Название дилера куда эвакуируют автомобиль")
       ]

sober = mkTransformField
       ["ТРЕЗВЫЙ ВОДИТЕЛЬ"]
       serviceField "sober" "services"
       [ ("fromAddress", Left "Адрес места поломки")
       , ("toAddress", Left "Адрес куда эвакуируют автомобиль")
       , ("multidrive", Right "1")
       ]

serviceTransformations = [ tech
                         , hotel
                         , towage1
                         , towage2
                         , towage3
                         , towage4
                         , taxi
                         , rent
                         , sober
                         ]


-- | How to remap columns of case.
caseMap :: FieldMap
caseMap = fixUtfMap $ map (\(k, v) -> (k, Right v)) $
          [ ("callDate", "Дата звонка")
          , ("callTime", "Время звонка")
          , ("callTaker", "Сотрудник РАМК (Обязательное поле)")
          , ("program", "Клиент (Обязательное поле)")
          , ("services", "Услуга (Обязательное поле)")
          , ("caller:callerName", "Фамилия звонящего")
          , ("caller:ownerName", "Фамилия владельца")
          , ("caller:phone", "Мобильный телефон")
          , ("status", "Статус звонка (Обязательное поле)")
          , ("car:plateNum", "Регистрационный номер автомобиля")
          , ("car:model", "Модель автомобиля")
          , ("car:color", "Цвет")
          , ("car:vin", "VIN автомобиля")
          , ("car:buyDate", "Дата покупки автомобиля")
          , ("car:mileage", "Пробег автомобиля (км)")
          , ("comment", "Описание неисправности со слов клиента")
          ]

-- | Build new commit from row and commit spec.
remapRow :: MapRow -> FieldMap -> MapRow
remapRow row cspec = M.mapWithKey 
                     (\k v -> case v of
                                Left val -> val
                                Right f -> maybe "" id (M.lookup f row))
                     cspec

-- | Try to build commit for dependent instance.
--
-- Return name of instance to be committed and its commit.
tryTransformation :: MapRow -> Transformation -> Maybe (ModelName, Commit)
tryTransformation row tr@(Transformation AlwaysMatch _ _ _) =
    Just (referenceModel tr,
          remapRow row $ commitMap tr)
tryTransformation row transform =
    if (maybe False (flip elem $ matchers $ match transform) 
                  (M.lookup (matchField $ match transform) row))
    then Just (referenceModel transform,
               remapRow row $ commitMap transform)
    else Nothing

-- | Post case entries directly to Redis using connection provided as
-- accumulator.
--
-- TODO: Split service / program / client references.
--
-- This is like 'mapIntoHandle', but for non-CSV output data.
caseAction :: ([FieldIndex], Connection) 
           -> ParsedRow MapRow 
           -> Iteratee B.ByteString IO ([FieldIndex], Connection)
caseAction h      (ParsedRow Nothing)  = return h
caseAction h       CSV.EOF             = return h
caseAction (idxs, c) (ParsedRow (Just r)) =
    let
      row = filterCaseRow r
      -- Commit only a fraction of original data for every case
      bareCommit = remapRow row caseMap
      -- Try all transformations and see which succeed
      trs = catMaybes $
            map (\t -> case (tryTransformation row t) of
                         Just r -> Just (t, r)
                         Nothing -> Nothing
                ) serviceTransformations
    in do
      liftIO $ runRedis c $ case trs of
        -- No transformations matched
        [] -> create "case" bareCommit idxs >> return (idxs, c)
        _ -> 
            do
              -- Store all dependant instances
              commit <- foldM (\c (trans, (depModel, depCommit)) -> do
                                 Right sid <- create depModel depCommit []
                                 return $ M.insert 
                                            (referenceField trans)
                                            (instanceKey depModel sid)
                                            c) bareCommit trs

              create "case" commit idxs
              return (idxs, c)

main :: IO ()
main =
    let
        sample = Options
                 { caseFile = def 
                   &= help "Path to CSV case archive file"
                   &= typFile
                 , index = [] &= help "Generate index on the field."
                 }
                 &= program "hmset-poster"
                 &= help "hmset-poster -c journal.csv -i field1 -i field2 ..."
    in do
      Options{..} <- cmdArgs $ sample
      case caseFile of
        Just fname -> do
          rConn <- connect defaultConnectInfo
          res <- foldCSVFile fname defCSVSettings caseAction 
                 (Prelude.map (\s -> (BU.fromString s, True)) index, rConn)
          case res of
            Left e -> error "Failed to process case archive"
            Right h -> return ()
        Nothing -> error "No file selected"
