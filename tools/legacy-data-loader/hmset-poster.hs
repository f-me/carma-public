{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Post legacy data directly to Redis hashes, cases only.

|-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.ByteString.Lazy.UTF8 as LBU (toString)
import Data.Enumerator as E hiding (map, head)
import qualified Data.Enumerator.Binary as EB

import Data.Maybe (catMaybes)
import qualified Data.Map as M hiding (map)

import Data.CSV.Enumerator as CSV

import Database.Redis

import System.Console.CmdArgs.Implicit

import Snap.Snaplet.Redson.CRUD
import Snap.Snaplet.Redson.Metamodel

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
type FieldMap = M.Map FieldName (Either FieldValue FieldName)

-- | Describes how to build new commit for dependant instance and how it
-- should be referenced from parent instance.
data Transformation =
    Transformation { matchers :: [FieldValue]
                   -- ^ legacy data service names to match this
                   -- transformation ("service" CSV field)

                   , matchField :: FieldName
                   -- ^ name of "service" field in CSV

                   , referenceModel :: ModelName
                   -- ^ reference to what model create on this commit

                   , referenceField :: FieldName
                   -- ^ what field of case model will hold the
                   -- <service>:<id> reference

                   , commitSpec :: FieldMap
                   }

-- | Map field names 1-to-1, no name fixing
bijectionMap :: [String] -> [(String, Either String String)]
bijectionMap fields = Prelude.map (\n -> (n, Right n)) fields

serviceField = "Услуга  (Обязательное поле)"

-- | Build transformation using string literals
--
-- TODO OverloadedStrings breaks UTF-8 literals.
mkTransformation :: [String] 
                 -> String
                 -> ModelName 
                 -> FieldName
                 -> [(String, Either String String)]
                 -> Transformation
mkTransformation matchers mfield model rfield spec = 
    Transformation (map BU.fromString matchers) (f mfield) model rfield $ 
                   M.fromList
                        -- TODO Find out how to use <$> here
                        (map (\(k, v) -> (f k, case v of
                                                 Left s -> Left $ f s
                                                 Right s -> Right $ f s))
                         spec)
        where f = BU.fromString

tech = mkTransformation
       ["ВСКРЫТИЕ ЗАМКОВ", "ДОСТАВКА ТОПЛИВА", 
        "ЗАМЕНА КОЛЁС", "ПОДЗАРЯДКА АКБ",
        "ПОДМЕННЫЙ АВТОМОБИЛЬ", "СНЯТИЕ С ПАРКИНГА",
        "ТЕХНИЧЕСКАЯ ПОМОЩЬ"] 
        serviceField "tech" "service"
       [ ("techType", Right "Услуга (Обязательное поле)")
       , ("caseAddress", Right "Адрес места поломки")
       , ("techContractor", Right "Название партнёра")
       , ("techComments", Right "Описание неисправности со слов клиента")
       ]

hotel = mkTransformation
       ["ГОСТИНИЦА"] 
       serviceField "hotel" "service"
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
towage1 = mkTransformation
          ["ЭВАКУАТОР", "ЭВАКУАЦИЯ В СЛУЧАЕ ДТП И ВАНДАЛИЗМА"]
          serviceField "towage" "service"
          $ towageCommonMap ++ [ ("towerType", Left "Эвакуатор")
                               , ("towType", Left "К дилеру")
                               ]

towage2 = mkTransformation
          ["ЭВАКУАЦИЯ ДО ДОМА В НОЧНОЕ ВРЕМЯ"] 
          serviceField "towage" "service"
          $ towageCommonMap ++ [ ("towerType", Left "Эвакуатор")
                               , ("towType", Left "К дому в ночное время")
                               ]

towage3 = mkTransformation
          ["ЭВАКУАЦИЯ ДО ШИНОМОНТАЖА"] 
          serviceField "towage" "service"
          $ towageCommonMap ++ [ ("towerType", Left "Эвакуатор")
                               , ("towType", Left "До мастерской")
                               ]

towage4 = mkTransformation
          ["МАНИПУЛЯТОР"]
          serviceField "towage" "service"
          $ towageCommonMap ++ [ ("towerType", Left "Манипулятор")
                               , ("towType", Left "К дилеру")
                               ]

serviceTransformations = [tech, hotel, towage1, towage2, towage3, towage4]

-- | Build new commit from row and commit spec.
remapRow row cspec = M.mapWithKey 
                     (\k v -> case v of
                                Left val -> val
                                Right f -> maybe "" id (M.lookup f row))
                     cspec

-- | Try to build commit for dependent instance.
--
-- Return name of instance to be committed and its commit.
tryTransformation :: MapRow -> Transformation -> Maybe (ModelName, Commit)
tryTransformation row transform =
    if (maybe False (flip elem $ matchers transform) 
                  (M.lookup (matchField transform) row))
    then Just (referenceModel transform,
               remapRow row $ commitSpec transform)
    else Nothing

-- | Post case entries directly to Redis using connection provided as
-- accumulator.
--
-- TODO: Split service / program / client references.
--
-- This is like 'mapIntoHandle', but for non-CSV output data.
caseAction :: ([FieldName], Connection) 
           -> ParsedRow MapRow 
           -> Iteratee B.ByteString IO ([FieldName], Connection)
caseAction h      (ParsedRow Nothing)  = return h
caseAction h       CSV.EOF             = return h
caseAction (i, c) (ParsedRow (Just r)) =
    let
      commit = filterCaseRow r
      -- Try all transformations and see which succeed
      trs = zip serviceTransformations
                (catMaybes $ map (\t -> tryTransformation commit t) 
                           serviceTransformations)
                
    in do
      liftIO $ runRedis c $ case trs of
        -- No transformations matched
        [] -> create "case" commit i >> return (i, c)
        -- Create dependant instance
        _ -> 
            let 
                (trans, (depModel, depCommit)) = head trs
            in
               do
                 Right sid <- create depModel depCommit []
                 create "case" (M.insert (referenceField trans)
                                 (instanceKey depModel sid)
                                 commit) i
                 return (i, c)

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
                 (Prelude.map BU.fromString index, rConn)
          case res of
            Left e -> error "Failed to process case archive"
            Right h -> return ()
        Nothing -> error "No file selected"
