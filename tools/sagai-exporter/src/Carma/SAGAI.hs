{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

{-|

  This module contains definitions of how to build every field of
  SAGAI entry, all bound together in 'sagaiFullExport' action.

  Exporting a case with services involves first fetching it from
  CaRMa, then using is as input data for 'runExport':

  > -- Fetch case
  > res <- readInstance cp "case" caseNumber
  > -- Fetch its services
  > servs <- forM (readReferences $ res M.! "services")
  >          (\(m, i) -> do
  >             inst <- readInstance cp m i
  >             return (m, i, inst))
  >
  > -- Perform export action on this data
  > fv <- runExport sagaiFullExport cnt (res, servs) cp wazzup

  Note that all services attached to the case must be supplied to
  'runExport', although some of them may not end up being in SAGAI entry.

  Final 'ExportState' as returned by 'runExport' may be used to keep
  @SEP@ counter value between runs.

-}

module Carma.SAGAI
    ( -- * Running SAGAI export
      sagaiFullExport
    , runExport
    -- * Export results
    , ExportState(..)
    , ExportError(..)
    , ErrorType(..)
    )

where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Aeson
import Data.Char
import Data.Dict as D
import Data.Functor
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding

import Data.Time.Clock
import Data.Time.Format
import System.Locale

import Network.HTTP

import Text.Printf

import Carma.HTTP

import Carma.SAGAI.Base
import Carma.SAGAI.Codes
import Carma.SAGAI.Util


-- | A common interface for both 'CaseExport' and 'ServiceExport'
-- monads used to define fields of an export entry. Most of fields
-- simply use this interface. Several have to be included in the class
-- due to different field content required for case and service
-- entries.
class (Functor m, Monad m, MonadIO m) => ExportMonad m where
    getCase        :: m InstanceData
    getAllServices :: m [Service]
    getCarmaPort   :: m Int
    getWazzup      :: m D.Dict
    -- | Return expense type for the case or currently processed
    -- service, depending on monad.
    expenseType    :: m ExpenseType
    -- | Stop export process due to critical error.
    exportError    :: ErrorType -> m a

    getState       :: m ExportState
    putState       :: ExportState -> m ()

    -- Fields for which export rules differ between case and services.
    panneField     :: m BS.ByteString
    defField       :: m BS.ByteString
    somField       :: m BS.ByteString
    comm3Field     :: m BS.ByteString


instance ExportMonad CaseExport where
    getCase = lift $ asks $ fst . fst

    getAllServices = lift $ asks $ snd . fst

    getCarmaPort = lift $ asks $ carmaPort . snd

    getWazzup = lift $ asks $ wazzup . snd

    exportError e = lift $ lift $ lift $ throwError $ CaseError e

    getState = get

    putState = put

    expenseType = do
      servs <- getAllServices
      case null servs of
        True -> return PhoneServ
        False -> do
          nonFalseServs <- getNonFalseServices
          return $ case null nonFalseServs of
                     True -> FalseCall
                     False -> Dossier

    panneField = cnst "0"

    defField = codeField defCode

    somField = padRight 10 '0' <$> codeField (formatCost . cost)

    comm3Field = do
      servs <- getNonFalseServices
      return $ case servs of
        [] -> BS.empty
        ((_, _, d):_) -> commentPad $ dataField0 "orderNumber" d


exportLog :: String -> CaseExport ()
exportLog s = lift $ lift $ tell [s]


instance ExportMonad ServiceExport where
    getCase = lift $ lift $ asks $ fst . fst

    getAllServices = lift $ lift $ asks $ snd . fst

    getCarmaPort = lift $ lift $ asks $ carmaPort . snd

    getWazzup = lift $ lift $ asks $ wazzup . snd

    exportError e = do
      (m, i, _) <- getService
      lift $ lift $ lift $ lift $ throwError $ ServiceError m i e

    expenseType = asks snd

    getState = lift $ get

    putState = lift . put

    panneField = cnst "1"

    defField = do
      et <- expenseType
      case et of
        -- Special handling for rental service DEF code.
        Rent ->
            do
              (_, _, d) <- getService
              return $ BS.append "G" $
                     padRight 2 '0' $ B8.pack $ show $ capRentDays d
        _ -> codeField defCode

    somField = do
        et <- expenseType
        padRight 10 '0' <$> case et of
          -- Special handling for rental service cost calculation.
          Rent ->
              do
                program <- caseField "program"
                (_, _, d) <- getService
                autoClass <- dataField1 "carClass" d
                let dailyCost =
                        case M.lookup (program, autoClass) rentCosts of
                          Just dc -> dc
                          -- Zero cost for unknown car classes.
                          Nothing -> 0
                return $ formatCost (dailyCost * (fromIntegral $ capRentDays d))
          _ -> codeField (formatCost . cost)

    comm3Field = do
        (mn, _, d) <- getService
        let oNum = dataField0 "orderNumber" d
        fields <-
            case mn of
              "tech" -> return [oNum]
              -- More fields are requred for towage/rental service
              _    ->
                  do
                    -- Try to fetch contractor code (not to be
                    -- confused with partner id) for selected
                    -- contractor
                    let partnerField =
                            case mn of
                              "rent"   -> "contractor_partnerId"
                              "towage" -> "towDealer_partnerId"
                              _        -> error "Never happens"
                        sPid = dataField0 partnerField d
                    cp <- getCarmaPort
                    pCode <- case (BS.null sPid, read1Reference sPid) of
                      -- If no partnerId specified, do not add partner
                      -- code to extra information to comm3 field.
                      (True, _) ->
                          return "#?"
                      (False, Nothing) ->
                          exportError (UnreadableContractorId sPid) >>
                          return ""
                      (False, Just (_, pid)) ->
                          dataField0 "code" <$>
                          (liftIO $ readInstance cp "partner" pid)
                    case mn of
                      "rent" ->
                          do
                            let pName =
                                    dataField0 "contractor_partner" d
                                vin   = dataField0 "vinRent" d
                                carCl = dataField0 "carClass" d
                            return [oNum, pCode, pName, vin, carCl]
                      "towage" -> return [oNum, pCode]
                      _        -> error "Never happens"
        return $ BS.intercalate " " fields


getService :: ServiceExport Service
getService = asks fst


-- | Calculate expense type for a service. Used to initialize
-- 'ServiceExport' state when processing nested services inside
-- 'CaseExport' monad.
serviceExpenseType :: Service -> CaseExport ExpenseType
serviceExpenseType (mn, _, d) = do
  case mn of
    "towage" ->
        do
          cid <- caseField1 "id"
          cp <- getCarmaPort
          liftIO $ do
                -- Check if this towage is a repeated towage using
                -- CaRMa HTTP method
                rs <- simpleHTTP $ getRequest $
                      methodURI cp $ "repTowages/" ++ (B8.unpack cid)
                rsb <- getResponseBody rs
                case (decode' $ BSL.pack rsb :: Maybe [Int]) of
                  Just [] -> return Towage
                  Just _  -> return RepTowage
                  -- TODO It's actually an error
                  Nothing -> return Towage
    "rent"   -> return Rent
    "tech"   -> do
            techType <- dataField1 "techType" d
            case techType of
              "charge"    -> return Charge
              "condition" -> return Condition
              "starter"   -> return Starter
              _           -> exportError $ UnknownTechType techType
    _        -> exportError $ UnknownService mn


-- | Get a value of a field in the case entry being exported.
-- Terminate export if field is not found.
dataField :: ExportMonad m => FieldName -> InstanceData -> m FieldValue
dataField fn d =
  case M.lookup fn d of
    Just fv -> return fv
    Nothing -> exportError $ NoField fn


-- | A version of 'dataField' which returns empty string if key is not
-- present in instance data (like 'M.findWithDefault')
dataField0 :: FieldName -> InstanceData -> FieldValue
dataField0 fn d = M.findWithDefault BS.empty fn d


-- | A version of 'dataField' which requires non-empty field value and
-- terminates with 'EmptyField' error otherwise.
dataField1 :: ExportMonad m => FieldName -> InstanceData -> m FieldValue
dataField1 fn d = do
  fv <- dataField fn d
  case BS.null fv of
    False -> return fv
    True -> exportError $ EmptyField fn


caseField :: ExportMonad m => FieldName -> m FieldValue
caseField fn = dataField fn =<< getCase


caseField1 :: ExportMonad m => FieldName -> m FieldValue
caseField1 fn = dataField1 fn =<< getCase


caseField0 :: ExportMonad m => FieldName -> m FieldValue
caseField0 fn = dataField0 fn <$> getCase


-- | Return all non-false services from services attached to the case.
getNonFalseServices :: ExportMonad m => m [Service]
getNonFalseServices = do
  servs <- getAllServices
  return $ filter notFalseService servs


-- | True if a service was not a false call (@falseCall@ field is
-- @none@).
notFalseService :: Service -> Bool
notFalseService (_, _, d) = dataField0 "falseCall" d == "none"


-- | True if service should be exported to SAGAI.
exportable :: Service -> Bool
exportable s@(mn, _, d) = notFalseService s && typeOk
    where typeOk =
              case mn of
                "towage" -> True
                "rent"   -> True
                "tech"   ->
                    elem (dataField0 "techType" d)
                             ["charge", "condition", "starter"]
                _        -> False


-- | Check if @callDate@ field of the case contains a date between dates
-- stored in two other case fields.
callDateWithin :: ExportMonad m =>
                  FieldName
               -- ^ Name of field with first date. @callDate@ must be
               -- not less that this.
               -> FieldName
               -- ^ @callDate@ must not exceed this.
               -> m Bool
callDateWithin f1 f2 = do
  ts1 <- caseField0 f1
  ts  <- caseField1 "callDate"
  ts2 <- caseField0 f2
  return $ case (parseTimestamp ts1,
                 parseTimestamp ts,
                 parseTimestamp ts2) of
    (Just t1, Just t, Just t2) -> t1 <= t && t < t2
    _ -> False


-- | Check if servicing contract is in effect.
onService :: ExportMonad m => m Bool
onService = callDateWithin "car_serviceStart" "car_serviceEnd"


-- | Check if warranty is in effect.
onWarranty :: ExportMonad m => m Bool
onWarranty = callDateWithin "car_warrantyStart" "car_warrantyEnd"


type ExportField = ExportMonad m => m BS.ByteString


cnst :: ExportMonad m => a -> m a
cnst = return


newline :: Char
newline = '\n'


space :: Char
space = ' '


rowBreak :: ExportField
rowBreak = return $ B8.singleton newline


pdvField :: ExportField
pdvField = do
  fv <- caseField1 "program"
  case fv of
    "peugeot" -> return "RUMC01R01"
    "citroen" -> return "FRRM01R01"
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


-- | Search for an entry in 'codesData' using program name of the case
-- and expense type, project the result to output.
codeField :: ExportMonad m =>
             (CodeRow -> BS.ByteString)
          -- ^ Projection function used to produce output from the
          -- matching 'codesData' entry.
          -> m BS.ByteString
codeField proj = do
  program <- caseField "program"
  key <- expenseType
  case M.lookup (program, key) codesData of
    Nothing -> exportError $ UnknownProgram program
    Just c -> return $ proj c


-- | Format 231.42 as "23142". Fractional part is truncated to 2
-- digits.
formatCost :: Double -> BS.ByteString
formatCost gc = BS.concat $ B8.split '.' $ B8.pack $ printf "%.2f" gc


impField :: ExportField
impField = do
  onS <- onService
  let proj = if onS
             then serviceImpCode
             else impCode
  codeField proj


causeField :: ExportField
causeField = padRight 15 '0' <$> codeField causeCode


-- | Extract properly capped amount of days from "providedFor" field
-- of instance data (used for rental service entries).
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


-- | Sequential counter.
composField :: ExportField
composField = do
  s <- getState
  let newCounter' = counter s + 1
      -- Wrap counter when it reaches 999999
      newCounter  = if newCounter' > 999999 then 0 else newCounter'
  putState $ s{counter = newCounter}
  return $ padRight 6 '0' $ B8.pack $ show $ counter s


ddgField :: ExportField
ddgField = do
  -- | First check servicing contract, then warranty.
  onS <- onService
  if onS
  then timestampToDate =<< caseField1 "car_serviceStart"
  else do
    onW <- onWarranty
    if onW
    then timestampToDate =<< caseField1 "car_warrantyStart"
    -- With current /psaCases implementation, this should not happen
    else spaces 6


ddrField :: ExportField
ddrField = timestampToDate =<< caseField1 "callDate"


ddcField :: ExportField
ddcField = do
  ctime <- liftIO $ getCurrentTime
  return $ B8.pack $ formatTime defaultTimeLocale dateFormat ctime


kmField :: ExportField
kmField = padRight 6 '0' <$> caseField "car_mileage"


spaces :: ExportMonad m => Int -> m BS.ByteString
spaces n = return $ B8.replicate n space


accordField :: ExportField
accordField = padRight 6 space <$> caseField0 "accord"


nhmoField :: ExportField
nhmoField = spaces 5


somprField :: ExportField
somprField = spaces 10


fillerField :: ExportField
fillerField = spaces 5


-- | Pad input up to 72 characters with spaces or truncate it to be
-- under 72 chars. Remove all newlines.
commentPad :: BS.ByteString -> BS.ByteString
commentPad = B8.map (\c -> if c == newline then space else c) .
             encodeUtf8 .
             T.take 72 .
             decodeUtf8 .
             padLeft 72 ' '


comm1Field :: ExportField
comm1Field = do
  val <- caseField1 "comment"
  d <- getWazzup
  case labelOfValue val d of
    Just label -> return $ commentPad label
    Nothing -> return val


comm2Field :: ExportField
comm2Field = commentPad <$> caseField0 "dealerCause"


-- | A list of field combinators to form a part of export entry.
type ExportPart = ExportMonad m => [m BS.ByteString]


-- | Basic part for any line.
basicPart :: ExportPart
basicPart =
    [ cnst "FGDC"
    , cnst "DM1"
    , pdvField
    , dtField
    , vinField
    , impField
    , causeField
    ]


-- | Common part for all non-comment lines (prior to @PANNE@ field).
refundPart :: ExportPart
refundPart =
    [ cnst "1"
    , composField
    , cnst " "
    , ddgField
    , ddrField
    , kmField
    , accordField
    , panneField
    , nhmoField
    , somField
    , somprField
    , defField
    , cnst "C"
    , ddcField
    , fillerField
    , rowBreak
    ]


comm1Part :: ExportPart
comm1Part =
    [ cnst "3"
    , comm1Field
    , rowBreak
    ]


comm2Part :: ExportPart
comm2Part =
    [ cnst "4"
    , comm2Field
    , rowBreak
    ]

comm3Part :: ExportPart
comm3Part =
    [ cnst "6"
    , comm3Field
    , rowBreak
    ]


-- | Defines all lines and fields in a full entry for the case.
entrySpec :: ExportPart
entrySpec = concat $
    [ basicPart
    , refundPart
    , basicPart
    , comm1Part
    , basicPart
    , comm2Part
    , basicPart
    , comm3Part
    ]


-- | Form an entry for the case or the service, depending on the
-- particular monad.
sagaiExport :: ExportMonad m => m BS.ByteString
sagaiExport = BS.concat <$> (mapM id entrySpec)


-- | Format service data as @towage:312@ (model name and id).
formatService :: Service -> String
formatService (mn, i, _) =  mn ++ ":" ++ show i


formatServiceList :: [Service] -> String
formatServiceList ss = "[" ++ (intercalate "," $ map formatService ss) ++ "]"


-- | Initialize 'ServiceExport' monad and run 'sagaiExport' in it for
-- a service.
runServiceExport :: Service -> CaseExport BS.ByteString
runServiceExport s = do
  exportLog $ "Now exporting " ++ formatService s
  et <- serviceExpenseType s
  exportLog $ "Expense type of service is " ++ show et
  runReaderT sagaiExport (s, et)


-- | Form a full entry for the case and its services (only those which
-- satisfy requirements defined by the spec), producing a ByteString.
sagaiFullExport :: CaseExport BS.ByteString
sagaiFullExport = do
  et <- expenseType
  exportLog $ "Expense type is " ++ show et
  caseOut <- sagaiExport

  allServs <- getAllServices
  let servs = filter exportable allServs
  exportLog $ "Case services: " ++ formatServiceList allServs ++
              ", exporting: " ++ formatServiceList servs
  servsOut <- BS.concat <$> mapM runServiceExport servs
  return $ BS.concat [caseOut, servsOut]
