{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}

{-|

  Monads and combinators used to describe export process.

-}

module Carma.SAGAI
    (-- * Export monad
      Export
    , runExport

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

import Data.Char
import Data.Dict as D
import Data.Functor
import qualified Data.Map as M

import Data.Time.Clock
import Data.Time.Format
import System.Locale

import Text.Printf

import Carma.HTTP
import Carma.SAGAI.Error
import Carma.SAGAI.Util


-- | A case instance and a list of service instances attached to the
-- case to be exported to SAGAI.
type ExportData = (InstanceData, [Service])


-- | Model name, id and data of a service.
type Service = (String, Int, InstanceData)


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


-- | Main monad used to form a SAGAI entry. Reader state stores case
-- and services to be exported. Error monad is provided to early
-- terminate entry export in case of critical errors. IO may be used
-- to query CaRMa database.
type Export =
    (StateT ExportState
     (ReaderT (ExportData, ExportOptions)
      (ErrorT ExportError IO)))


getCase = lift $ asks $ fst . fst


getServices :: Export [Service]
getServices = lift $ asks $ snd . fst


getWazzup :: Export D.Dict
getWazzup = lift $ asks $ wazzup . snd


-- | Perform export action using the provided case and services data
-- and export options.
runExport :: Export a
          -> Int
          -- ^ Initial value for @SEP@ line counter.
          -> ExportData
          -> Int
          -- ^ CaRMa port.
          -> D.Dict
          -- ^ Wazzup dictionary.
          -> IO (Either ExportError a)
runExport act sepStart input cp wz = do
    res <- runErrorT $
           runReaderT (runStateT act $ ExportState sepStart) $
           (input, ExportOptions cp wz)
    return $ fst <$> res



exportError e = lift $ lift $ throwError e


caseExpenseType = do
  servs <- getServices
  case null servs of
    True -> return PhoneServ
    False -> do
      nonFalseServs <- getNonFalseServices
      return $ case null nonFalseServs of
                 True -> FalseCall
                 False -> Dossier


servExpenseType :: Service -> Export ExpenseType
servExpenseType (mn, _, d) = do
  case mn of
    -- | TODO Add RepTowage branch
    "towage" -> return Towage
    "rent"   -> return Rent
    "tech"   -> do
            techType <- dataField1 "techType" d
            case techType of
              "charge"    -> return Charge
              "condition" -> return Condition
              "starter"   -> return Starter
              _           -> exportError $ UnknownTechType techType


-- | A version of 'dataField' which returns empty string if key is not
-- present in instance data (like 'M.findWithDefault')
dataField0 :: FieldName -> InstanceData -> FieldValue
dataField0 fn d = M.findWithDefault BS.empty fn d


-- | Get a value of a field in the case entry being exported.
-- Terminate export if field is not found.
dataField :: FieldName -> InstanceData -> Export FieldValue
dataField fn d =
  case M.lookup fn d of
    Just fv -> return fv
    Nothing -> exportError $ NoField fn


-- | A version of 'dataField' which requires non-empty field value.
dataField1 :: FieldName -> InstanceData -> Export FieldValue
dataField1 fn d = do
  fv <- dataField fn d
  case BS.null fv of
    False -> return fv
    True -> exportError $ EmptyField fn


caseField fn = getCase >>= dataField fn
caseField1 fn = dataField1 fn =<< getCase
caseField0 fn = dataField0 fn <$> getCase


-- | Return all non-false services from services attached to the case.
getNonFalseServices :: Export [Service]
getNonFalseServices = do
  servs <- getServices
  return $ filter (\x@(m, i, d) ->
                       dataField0 "falseCall" d == "none") servs


-- | Check if @callDate@ field of the case contains a date between dates
-- stored in two other case fields.
callDateWithin :: FieldName
               -- ^ Name of field with first date. @callDate@ must exceed this.
               -> FieldName 
               -- ^ @callDate@ must not exceed this.
               -> Export Bool
callDateWithin f1 f2 = do
  ts1 <- caseField0 f1
  ts  <- caseField1 "callDate"
  ts2 <- caseField0 f2
  return $ case (parseTimestamp ts1,
                 parseTimestamp ts,
                 parseTimestamp ts2) of
    (Just t1, Just t, Just t2) -> t1 <= t && t > t2
    _ -> False


-- | Check if servicing contract is in effect.
onService = callDateWithin "car_serviceStart" "car_serviceStop"


-- | Check if warranty is in effect.
onWarranty = callDateWithin "car_warrantyStart" "car_warrantyStop"


type ExportField = Export BS.ByteString


cnst :: a -> Export a
cnst = return


newline :: ExportField
newline = return $ B8.singleton '\n'


pdvField :: ExportField
pdvField = do
  fv <- caseField1 "program"
  case fv of
    "peugeot" -> return $ padRight 9 ' ' "RUMC01R"
    "citroen" -> return $ padRight 9 ' ' "FRRM01R"
    _         -> exportError $ UnknownProgram fv


dtField :: ExportField
dtField = padRight 8 '0' <$> caseField1 "id"


vinField :: ExportField
vinField = do
  bs <- caseField1 "car_vin"
  return $ B8.pack $ map toUpper $ B8.unpack bs


dateFormat :: String
dateFormat = "%d%m%y"


-- | Convert timestamp to DDMMYY format.
timestampToDate :: BS.ByteString -> ExportField
timestampToDate input =
    case parseTimestamp input of
      Just time ->
          return $ B8.pack $ formatTime defaultTimeLocale dateFormat time
      Nothing -> exportError $ BadTime input


data ExpenseType = Dossier
                 | FalseCall
                 | PhoneServ
                 | Charge
                 | Condition
                 | Starter
                 | Towage
                 | RepTowage
                 | Rent
                   deriving (Eq, Ord)


-- | Data for particular type of expense.
data CodeRow = CodeRow { cost             :: Double
                       , impCode          :: BS.ByteString
                       , serviceImpCode   :: BS.ByteString
                       , causeCode        :: BS.ByteString
                       , defCode          :: BS.ByteString
                       }


-- | List of costs and I/D/C codes for all programs and expenses.
codesData :: M.Map (FieldValue, ExpenseType) CodeRow
codesData = M.fromList
    [ (("citroen", Dossier),     CodeRow 1148    "DV1" "DV4" "9938" "G5F")
    , (("citroen", FalseCall),   CodeRow 574     "DR1" "DR4" "9939" "296")
    , (("citroen", PhoneServ),   CodeRow 351     "DV1" "DV4" "9939" "G5F")
    , (("citroen", Charge),      CodeRow 1825    "DR1" "DR4" "996L" "446")
    , (("citroen", Condition),   CodeRow 1825    "DR1" "DR4" "996D" "446")
    , (("citroen", Starter),     CodeRow 1825    "DR1" "DR4" "996A" "446")
    , (("citroen", Towage),      CodeRow 2911    "DR1" "DR4" "9934" "G5F")
    , (("citroen", RepTowage),   CodeRow 2009    "DR1" "DR4" "9936" "G5F")
    , (("citroen", Rent),        CodeRow 0       "PV1" "PV4" "9927" "PZD")
    , (("peugeot", Dossier),     CodeRow 1354.64 "24R" "FCA" "8999" "G5D")
    , (("peugeot", FalseCall),   CodeRow 677.32  "24E" "FCA" "8990" "G5D")
    , (("peugeot", PhoneServ),   CodeRow 414.18  "24E" "FCA" "8943" "G5D")
    , (("peugeot", Charge),      CodeRow 2153.5  "24E" "FCA" "8962" "G5D")
    , (("peugeot", Condition),   CodeRow 2153.5  "24E" "FCA" "8954" "G5D")
    , (("peugeot", Starter),     CodeRow 2153.5  "24E" "FCA" "8963" "G5D")
    , (("peugeot", Towage),      CodeRow 3434.98 "24E" "FCA" "8950" "G5D")
    , (("peugeot", RepTowage),   CodeRow 2370.62 "2RE" "FCA" "8983" "G5D")
    , (("peugeot", Rent),        CodeRow 0       "F6R" "FCA" "8997" "PZD")
    ]


-- | Daily costs for car rent service.
rentCosts :: M.Map (FieldValue, FieldValue) Double
rentCosts = M.fromList
    [ (("citroen", "psab"),  1758)
    , (("citroen", "psam1"), 2310)
    , (("citroen", "psam2"), 3041)
    , (("citroen", "psah"),  3994)
    , (("citroen", "psam"),  2310)
    , (("peugeot", "psab"),  2074.44)
    , (("peugeot", "psam1"), 2725.8)
    , (("peugeot", "psam2"), 3588.38)
    , (("peugeot", "psah"),  4712.92)
    , (("peugeot", "psam"),  2725.8)
    ]


-- | Search for an entry in 'codesData' using program and expense
-- type, project the result to output.
codeField :: Export ExpenseType
          -- ^ Action used to choose an expense type.
          -> (CodeRow -> BS.ByteString)
          -- ^ Projection function used to produce output from the
          -- matching 'codesData' entry.
          -> Export BS.ByteString
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


caseImpField :: ExportField
caseImpField = codeField caseExpenseType impCode


servImpField :: Service -> ExportField
servImpField s = codeField (servExpenseType s) impCode


caseCauseField :: ExportField
caseCauseField =
    padRight 15 '0' <$> codeField caseExpenseType causeCode


servCauseField :: Service -> ExportField
servCauseField s =
    padRight 15 '0' <$> codeField (servExpenseType s) causeCode


caseDefField :: ExportField
caseDefField = codeField caseExpenseType defCode



-- | Extract properly capped amount of days from "providedFor" field
-- of instance data.
capRentDays :: InstanceData -> Int
capRentDays d = do
  -- Cap days to be within [0..4]
  case B8.readInt $ dataField0 "providedFor" d of
    Just (n, _) ->
        if n > 4
        then 4
        else if n < 0
             then 0
             else n
    Nothing -> 0


servDefField :: Service -> ExportField
servDefField s@(_, _, d) = do
  et <- servExpenseType s
  case et of
    -- Special handling for rental service DEF code.
    Rent -> 
        do
          return $ BS.append "G" $ padRight 2 '0' $ B8.pack $ show $ capRentDays d
    _ -> codeField (servExpenseType s) defCode


caseSomField :: ExportField
caseSomField =
    padRight 10 '0' <$> codeField caseExpenseType (formatCost . cost)


servSomField :: Service -> ExportField
servSomField s@(_, _, d) = do
    et <- servExpenseType s
    padRight 10 '0' <$> case et of
      Rent -> 
          do
            program <- caseField "program"
            autoClass <- dataField1 "carClass" d
            let dailyCost = 
                    case M.lookup (program, autoClass) rentCosts of
                      Just dc -> dc
                      -- Zero cost for unknown car classes.
                      Nothing -> 0
            return $ formatCost (dailyCost * (fromIntegral $ capRentDays d))
      _ -> codeField (servExpenseType s) (formatCost . cost)


sepField :: ExportField
sepField = do
  st <- get
  put st{counter = counter st + 1}
  return $ padRight 6 '0' $ B8.pack $ show $ counter st


-- | First check servicing contract, then warranty.
ddgField :: ExportField
ddgField = do
  onS <- onService
  if onS 
  then timestampToDate =<< caseField1 "car_serviceStart"
  else do
    onW <- onWarranty
    if onW
    then timestampToDate =<< caseField1 "car_warrantyStart"
    else return ""


ddrField :: ExportField
ddrField = timestampToDate =<< caseField1 "callDate"


ddcField :: ExportField
ddcField = do
  ctime <- liftIO $ getCurrentTime
  return $ B8.pack $ formatTime defaultTimeLocale dateFormat ctime


kmField = padRight 6 '0' <$> caseField "car_mileage"


spaces n = return $ B8.replicate n ' '


accordField = padRight 6 ' ' <$> caseField0 "accord"


nhmoField = spaces 5


somprField = spaces 10


fillerField = spaces 5


commentPad = padLeft 72 ' '


comm1Field :: ExportField
comm1Field = do
  val <- caseField1 "comment"
  d <- getWazzup
  case labelOfValue val d of
    Just label -> return $ commentPad label
    Nothing -> exportError $ UnknownComment val


comm2Field = commentPad <$> caseField0 "dealerCause"


comm3CaseField :: ExportField
comm3CaseField = do
  servs <- getNonFalseServices
  return $ case servs of
    [] -> BS.empty
    ((m, i, d):_) -> commentPad $ dataField0 "orderNumber" d


comm3ServField :: Service -> ExportField
comm3ServField (m, _, d) =
    -- | Properly query partner data
    case m of
      _ -> return $ commentPad $ dataField0 "orderNumber" d


-- | Basic part for any line.
basicPart =
    [ cnst "FGDC"
    , cnst "DM1"
    , pdvField
    , dtField
    , vinField
    ]


-- | Common part for all non-comment lines (prior to @PANNE@ field).
refundPart =
    [ cnst "1"
    , sepField
    , cnst " "
    , ddgField
    , ddrField
    , kmField
    , accordField
    ]


casePanneField = cnst "0"


servPanneField = cnst "1"


-- | Common part for all non-comment lines (after @DEFAUT@ field).
refundTailPart =
    [ cnst "C"
    , ddcField
    , fillerField
    ]


comm1Part =
    [ cnst "3"
    , comm1Field
    , newline
    ]


comm2Part =
    [ cnst "4"
    , comm2Field
    , newline
    ]


comm3CasePart =
    [ cnst "6"
    , comm3CaseField
    , newline
    ]


comm3ServPart s =
    [ cnst "6"
    , comm3ServField s
    , newline
    ]

-- | Form a full entry for the case and its services.
caseEntry :: Export BS.ByteString
caseEntry = do
  let caseFrontPart = [ basicPart
                      , [ caseImpField
                        , caseCauseField
                        ]
                      ]
  casePart <- BS.concat <$>
        (mapM id $ concat $
         caseFrontPart ++ [ refundPart
                          , [ casePanneField
                            , nhmoField
                            , caseSomField
                            , somprField
                            , caseDefField
                            ]
                          , refundTailPart
                          , [ newline ]
                          ] ++
         caseFrontPart ++ [ comm1Part     ] ++
         caseFrontPart ++ [ comm2Part     ] ++
         caseFrontPart ++ [ comm3CasePart ]
        )

  -- Export all non-empty services too
  servs <- getNonFalseServices
  servsPart <- BS.concat <$> mapM servEntry servs
  return $ BS.append casePart servsPart



servEntry :: Service -> Export BS.ByteString
servEntry s = do
  let servFrontPart = [ basicPart
                      , [ servImpField s
                        , servCauseField s
                        ]
                      ]
  BS.concat <$>
        (mapM id $ concat $
         servFrontPart ++ [ refundPart
                          , [ servPanneField
                            , nhmoField
                            , servSomField s
                            , somprField
                            , servDefField s
                            ]
                          , refundTailPart
                          , [ newline ]
                          ] ++
         servFrontPart ++ [ comm1Part       ] ++
         servFrontPart ++ [ comm2Part       ] ++
         servFrontPart ++ [ comm3ServPart s ]
        )
