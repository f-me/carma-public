{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

  Monads and combinators used to describe export process.

-}

module Carma.SAGAI
    (-- * Export monad
      CaseEntry
    , runCaseExport

    -- * Export combinators
    -- ** Basic
    , caseField
    , caseField1
    -- ** SAGAI format-specific combinators
    , pdvField

    -- * Primary export routines
    , caseEntry
    )

where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

import Data.Dict as D
import Data.Functor
import qualified Data.Map as M

import Data.Time.Clock
import Data.Time.Format
import System.Locale

import Text.Printf

import Carma.HTTP
import Carma.SAGAI.Error




-- | A case instance and a list of service instances attached to the
-- case to be exported to SAGAI.
type ExportData = (InstanceData, [ServiceEntry])


-- | Model name, id and data of a service.
type ServiceEntry = (String, Int, InstanceData)


data ExportState = ExportState { counter :: Int
                               -- ^ Current entry line counter used
                               -- for @SEP@ field.
                               }


-- | Read only options used when processing a case.
data ExportOptions = ExportOptions { carmaPort :: Int
                                   -- ^ CaRMa port.
                                   , wazzup :: D.Dict
                                   -- ^ Dictionary used on the @comment@
                                   -- field of a case.
                                   }


-- | Monad used to form a SAGAI entry for a case. Reader state stores
-- case and service to be exported. Error monad is provided to early
-- terminate entry export in case of critical errors. IO may be used
-- to query CaRMa database.
type CaseEntry =
    (StateT ExportState
     (ReaderT (ExportData, ExportOptions)
      (ErrorT ExportError IO)))


getCase :: CaseEntry InstanceData
getCase = lift $ asks $ fst . fst


getServices :: CaseEntry [ServiceEntry]
getServices = lift $ asks $ snd . fst


getWazzup :: CaseEntry D.Dict
getWazzup = lift $ asks $ wazzup . snd


exportError e = lift $ lift $ throwError e


-- | Perform given actions in context of provided case and services
-- data.
runCaseExport :: CaseEntry a
              -> Int
              -- ^ Initial value for @SEP@ line counter.
              -> ExportData
              -> Int
              -- ^ CaRMa port.
              -> D.Dict
              -- ^ Wazzup dictionary.
              -> IO (Either ExportError a)
runCaseExport act sepStart input cp wz = do
    res <- runErrorT $ 
           runReaderT (runStateT act $ ExportState sepStart) $
           (input, ExportOptions cp wz)
    return $ fst <$> res


-- | Get a value of a field in the case entry being exported.
-- Terminate export if field is not found.
dataField :: FieldName -> InstanceData -> CaseEntry FieldValue
dataField fn d = do
  case M.lookup fn d of
    Just fv -> return fv
    Nothing -> exportError $ NoField fn


-- | A version of 'caseField1' which requires non-empty field value.
dataField1 :: FieldName -> InstanceData -> CaseEntry FieldValue
dataField1 fn d = do
  fv <- dataField fn d
  case BS.null fv of
    False -> return fv
    True -> exportError $ EmptyField fn

caseField fn = getCase >>= dataField fn
caseField1 fn = getCase >>= dataField1 fn


type CaseField = CaseEntry BS.ByteString


cnst :: a -> CaseEntry a
cnst = return


newline :: CaseField
newline = return $ B8.singleton '\n'


-- | Return string required to pad input up to provided length. If
-- input is already not less than required length, return empty
-- string.
genericPad :: Int
           -- ^ Required result length.
           -> Char
           -- ^ Padding symbol.
           -> BS.ByteString
           -- ^ Input string.
           -> BS.ByteString
genericPad padLen pad input =
    if len < padLen
    then (B8.replicate (padLen - len) pad)
    else BS.empty
    where
      len = BS.length input


-- | Pad input using 'genericPad', keeping original string to the right.
padRight :: Int
         -- ^ Minimal string length.
         -> Char
         -- ^ Padding symbol.
         -> BS.ByteString
         -- ^ Input string.
         -> BS.ByteString
padRight padLen pad input = BS.append (genericPad padLen pad input) input


-- | Pad input using 'genericPad', keeping original string to the left.
padLeft :: Int
        -- ^ Minimal string length.
        -> Char
        -- ^ Padding symbol.
        -> BS.ByteString
        -- ^ Input string.
        -> BS.ByteString
padLeft padLen pad input = BS.append input (genericPad padLen pad input)


pdvField :: CaseField
pdvField = do
  fv <- caseField1 "program"
  case fv of
    "peugeot" -> return $ padRight 9 ' ' "RUMC01R"
    "citroen" -> return $ padRight 9 ' ' "FRRM01R"
    _         -> exportError $ UnknownProgram fv


dtField :: CaseField
dtField = padRight 8 '0' <$> caseField1 "id"


vinField :: CaseField
vinField = caseField1 "car_vin"


dateFormat :: String
dateFormat = "%d%m%y"


-- | Convert timestamp to DDMMYY format.
timestampToDate :: BS.ByteString -> CaseField
timestampToDate input =
    case parseTime defaultTimeLocale "%s" (B8.unpack input) of
      (Just time :: Maybe UTCTime) ->
          return $ B8.pack $ formatTime defaultTimeLocale dateFormat time
      Nothing -> exportError $ BadTime input


caseExpenseType :: CaseEntry ExpenseType
caseExpenseType = do
  servs <- getServices
  return $ case null servs of
             True -> PhoneServ
             -- TODO Add FalseCall branch
             False -> Dossier


data ExpenseType = Dossier
                 | FalseCall
                 | PhoneServ
                 | Charge
                 | Condition
                 | Starter
                 | Evac
                 | RepEvac
                 | Rent
                   deriving (Eq, Ord)


-- | Data for particular type of expense.
data CodeRow = CodeRow { cost      :: Double
                       , impCode   :: BS.ByteString
                       , causeCode :: BS.ByteString
                       , defCode   :: BS.ByteString
                       }


-- | List of costs and I/D/C codes for all programs and expenses.
codesData :: M.Map (FieldValue, ExpenseType) CodeRow
codesData = M.fromList
    [ (("citroen", Dossier),     CodeRow 1148    "DV1" "9938" "G5F")
    , (("citroen", FalseCall),   CodeRow 574     "DR1" "9939" "296")
    , (("citroen", PhoneServ),   CodeRow 351     "DV1" "9939" "GF5")
    , (("citroen", Charge),      CodeRow 1825    "DR1" "996L" "446")
    , (("citroen", Condition),   CodeRow 1825    "DR1" "996D" "446")
    , (("citroen", Starter),     CodeRow 1825    "DR1" "996A" "446")
    , (("citroen", Evac),        CodeRow 2911    "DR1" "9934" "G5F")
    , (("citroen", RepEvac),     CodeRow 2009    "DR1" "9936" "G5F")
    , (("citroen", Rent),        CodeRow 0       "PV1" "9927" "PZD")
    , (("peugeot", Dossier),     CodeRow 1354.64 "24R" "8999" "G5D")
    , (("peugeot", FalseCall),   CodeRow 677.32  "24E" "8990" "G5D")
    , (("peugeot", PhoneServ),   CodeRow 414.18  "24E" "8943" "G5D")
    , (("peugeot", Charge),      CodeRow 2153.5  "24E" "8962" "G5D")
    , (("peugeot", Condition),   CodeRow 2153.5  "24E" "8954" "G5D")
    , (("peugeot", Starter),     CodeRow 2153.5  "24E" "8963" "G5D")
    , (("peugeot", Evac),        CodeRow 3434.98 "24E" "8950" "G5D")
    , (("peugeot", RepEvac),     CodeRow 2370.62 "2RE" "8983" "G5D")
    , (("peugeot", Rent),        CodeRow 0       "F6R" "8997" "PZD")
    ]


-- | Search for an entry in 'codesData' using program and expense
-- type, project the result to output.
codeField :: CaseEntry ExpenseType
          -- ^ Action used to choose an expense type.
          -> (CodeRow -> BS.ByteString)
          -- ^ Projection function used to produce output from 'codesData' entry.
          -> CaseField
codeField expType proj = do
  program <- caseField "program" 
  key <- expType
  case M.lookup (program, key) codesData of
    Nothing -> exportError $ UnknownProgram program
    Just c -> return $ proj c


-- | Format 231.42 as "23142". Fractional part is truncated to 2
-- digits.
formatCost :: Double -> BS.ByteString
formatCost gc = BS.concat $ B8.split '.' $ B8.pack $ printf "%.2f" gc


caseImpField :: CaseField
caseImpField = codeField caseExpenseType impCode


caseCauseField :: CaseField
caseCauseField =
    padRight 15 '0' <$> codeField caseExpenseType causeCode


caseDefField :: CaseField
caseDefField = codeField caseExpenseType defCode


caseSomField :: CaseField
caseSomField =
    padRight 10 '0' <$> codeField caseExpenseType (formatCost . cost)


sepField :: CaseField
sepField = do
  st <- get
  put st{counter = counter st + 1}
  return $ padRight 6 '0' $ B8.pack $ show $ counter st


-- | TODO: Fix ddgField
ddgField :: CaseField
ddgField = cnst "010112"
--ddgField = timestampToDate =<< caseField1 "car_serviceStart"


ddrField :: CaseField
ddrField = timestampToDate =<< caseField1 "callDate"


ddcField :: CaseField
ddcField = do
  ctime <- liftIO $ getCurrentTime
  return $ B8.pack $ formatTime defaultTimeLocale dateFormat ctime


kmField = padRight 6 '0' <$> caseField "car_mileage"


spaces n = return $ B8.replicate n ' '


accordField = spaces 6


nhmoField = spaces 5


somprField = spaces 10


fillerField = spaces 5


comm1Field :: CaseField
comm1Field = do
  val <- caseField1 "comment"
  d <- getWazzup
  case labelOfValue val d of
    Just label -> return label
    Nothing -> exportError $ UnknownComment val


comm2Field = padLeft 72 ' ' <$> caseField "dealerCause"


-- | TODO: Finish this (query services list)
comm3CaseField = comm2Field


-- | Basic part for any line.
basicPart =
    [ cnst "FGDC"
    , cnst "DM1"
    , pdvField
    , dtField
    , vinField
    ]


-- | Common part for non-comment lines (prior to @PANNE@ field).
refundPart =
    [ cnst "1"
    , sepField
    , cnst " "
    , ddgField
    , ddrField
    , kmField
    , accordField
    ]


-- | Common part for non-comment lines (after @DEFAUT@ field).
refundTailPart =
    [ cnst "C"
    , ddcField
    , fillerField
    ]


comm1Part =
    [ cnst "3"
    , comm1Field
    ]


comm2Part =
    [ cnst "4"
    , comm2Field
    ]


comm3CasePart =
    [ cnst "6"
    , comm3CaseField
    ]


-- | Form a full entry for a case.
--
-- TODO Export services
caseEntry :: CaseField
caseEntry =
    let
        caseFrontPart = [ basicPart
                        , [ caseImpField
                          , caseCauseField
                          ]
                        ]
    in
      BS.concat <$>
            (mapM id $ concat $
             caseFrontPart ++ [ refundPart
                              , [ cnst "0"
                                , nhmoField
                                , caseSomField
                                , somprField
                                , caseDefField
                                ]
                              , refundTailPart
                              , [ newline
                                ]
                              ] ++
             caseFrontPart ++ [ comm1Part,     [ newline ] ] ++
             caseFrontPart ++ [ comm2Part,     [ newline ] ] ++
             caseFrontPart ++ [ comm3CasePart, [ newline ] ]
            )
