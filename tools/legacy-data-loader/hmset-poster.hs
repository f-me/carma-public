{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Post legacy data directly to Redis hashes, cases only.

TODO More row transformations.

TODO Support models other than "case".

TODO Support more than one dependant form.

|-}

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Arrow (second)
import Control.Concurrent
import Control.Concurrent.QSem

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU (fromString, toString)

import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as M

import Data.Enumerator as E hiding (foldM, map, head, length)
import Data.CSV.Enumerator as CSV
import qualified Data.Aeson as Aeson

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format

import System.Console.CmdArgs.Implicit
import System.Locale

import RedsonTypes
import RESTLoader



data Options = Options
    { caseFile :: Maybe FilePath
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


-- | Describes how to build instance fields from CSV row.

-- Left field value is read as is while Right indicates the field of
-- original row to populate the new instance field from.
type FieldMap = M.Map FieldName FieldSource

-- | Describes how every field is constructed from CSV row.
data FieldSource = Value FieldValue
                 -- ^ Just set field value as is
                 | FromField FieldName
                 -- ^ Take value of a named field in original row
                 | Function (MapRow -> FieldValue)
                 -- ^ Build field value by applying a function to the
                 -- whole row

-- 'FieldMap' with unfixed strings, without generic transformations
-- and in list form
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
                 (map (\(k, v) -> (f k,
                                   case v of
                                     Left s -> Value $ f s
                                     Right s -> FromField $ f s))
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
caseMap = let 
    plain = fixUtfMap $ map (second Right)
          [ ("callTaker", "Сотрудник РАМК (Обязательное поле)")
          , ("program", "Клиент (Обязательное поле)")
          , ("services", "Услуга (Обязательное поле)")
          , ("caller_name", "Фамилия звонящего")
          , ("owner_name", "Фамилия владельца")
          , ("caller_phone1", "Мобильный телефон")
          , ("status", "Статус звонка (Обязательное поле)")
          , ("car_plateNum", "Регистрационный номер автомобиля")
          , ("car_model", "Модель автомобиля")
          , ("car_color", "Цвет")
          , ("car_vin", "VIN автомобиля")
          , ("car_mileage", "Пробег автомобиля (км)")
          , ("address_address", "Адрес места поломки")
          , ("comment", "Описание неисправности со слов клиента")
          ]
    locale = defaultTimeLocale
    sourceFmt = "%m/%d/%Y"
    -- Convert MM/DD/YYYY to POSIX seconds
    mkBuyDate mr = BU.fromString $ show $ fromMaybe 0 $ do
        dStr <- BU.fromString "Дата покупки автомобиля" `M.lookup` mr
        dUTC <- parseTime locale sourceFmt (BU.toString dStr) :: Maybe UTCTime
        return $ round $ utcTimeToPOSIXSeconds dUTC

    mkCallDate mr = BU.fromString $ show $ fromMaybe 0 $ do
        dStr <- BU.fromString "Дата звонка" `M.lookup` mr
        dUTC <- parseTime locale sourceFmt (BU.toString dStr) :: Maybe UTCTime
        let dInt = round $ utcTimeToPOSIXSeconds dUTC
        -- read time and convert it to number of seconds since midnight
        -- we need this because '%R' format allows only zero padded values
        let tInt = fromMaybe 0 $ do
              tStr <- BU.fromString "Время звонка" `M.lookup` mr
              (h,mStr) <- B.readInt tStr
              (m,"")   <- B.readInt $ B.tail mStr
              return $ (h*60 + m)*60
        return $ dInt + tInt
    in
      M.insert "car_buyDate" (Function mkBuyDate) $
        M.insert "callDate" (Function mkCallDate) $
           plain



-- | Build new commit from row and commit spec.
remapRow :: MapRow -> FieldMap -> MapRow
remapRow row cspec = M.mapWithKey 
                     (\k v -> case v of
                                Value val -> val
                                FromField f -> maybe "" id (M.lookup f row)
                                Function f -> f row)
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
caseAction :: BrowserSt
           -> MapRow 
           -> IO ()
caseAction h r =
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
    in
      case trs of
        -- No transformations matched
        [] -> void $ create h "case" bareCommit
        _  -> do
              -- Store all dependant instances
              commit <- foldM (\c (trans, (depModel, depCommit)) -> do
                                 Right sid <- create h depModel depCommit
                                 return $ M.insert 
                                            (referenceField trans)
                                            (instanceKey depModel sid)
                                            c) bareCommit trs
              void $ create h "case" commit

mapP :: Int -> (a -> IO ()) -> [a] -> IO ()
mapP _ _ [] = return ()
mapP n f xs =
    mapP_ hs >> mapP n f ts
  where
    (hs, ts) = splitAt n xs

    mapP_ ys =
      do
        sem <- newQSem $ length ys
        mapM_ (\a -> forkIO $ f a >> signalQSem sem) ys
        waitQSem sem

main :: IO ()
main =
    let
        sample = Options
                 { caseFile = def 
                   &= help "Path to CSV case archive file"
                   &= typFile
                 }
                 &= program "hmset-poster"
                 &= help "hmset-poster -c journal.csv"
    in do
      Options{..} <- cmdArgs sample
      case caseFile of
        Nothing -> error "No file selected"
        Just fname -> do
            w <- login
            res <- readCSVFile defCSVSettings fname
            case res of
              Left err -> print err
              Right rows -> mapP 100 (caseAction w) rows

