{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


{-|

  Monads and combinators used to describe export process.

-}

module Carma.SAGAI
    ( CaseExport
    , ExportState(..)
    , runExport
    , sagaiExport
    , sagaiFullExport
    , ExportError(..)
    )

where

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
import Carma.SAGAI.Codes
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


-- | Main monad used to form a SAGAI entry for a case. Reader state
-- stores the case and its services. Error monad is provided to early
-- terminate entry export in case of critical errors. IO may be used
-- to query CaRMa database.
type CaseExport =
    (StateT ExportState
     (ReaderT (ExportData, ExportOptions)
      (ErrorT ExportError IO)))


-- | A sub-monad used when forming a part of a SAGAI entry
-- corresponding to a service. Provides easy access to the currently
-- processed service.
type ServiceExport =
    ReaderT Service CaseExport


-- | A common interface for both 'CaseExport' and 'ServiceExport'
-- monads used to define fields of an export entry. Most of fields are
-- simply use this interface. Several have to be included in the class
-- due to different field content required for case and service
-- entries.
class (Functor m, Monad m, MonadIO m) => ExportMonad m where
    getCase        :: m InstanceData
    getAllServices :: m [Service]
    getCarmaPort   :: m Int
    getWazzup      :: m D.Dict
    expenseType    :: m ExpenseType
    exportError    :: ExportError -> m a

    getState       :: m ExportState
    putState       :: ExportState -> m ()

    panneField     :: m BS.ByteString
    defField       :: m BS.ByteString
    somField       :: m BS.ByteString
    comm3Field     :: m BS.ByteString

instance ExportMonad CaseExport where
    getCase = lift $ asks $ fst . fst

    getAllServices = lift $ asks $ snd . fst

    getCarmaPort = lift $ asks $ carmaPort . snd

    getWazzup = lift $ asks $ wazzup . snd

    exportError e = lift $ lift $ throwError e

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


instance ExportMonad ServiceExport where
    getCase = lift $ lift $ asks $ fst . fst

    getAllServices = lift $ lift $ asks $ snd . fst

    getCarmaPort = lift $ lift $ asks $ carmaPort . snd

    getWazzup = lift $ lift $ asks $ wazzup . snd

    exportError e = lift $ lift $ lift $ throwError e

    expenseType = do
      (mn, _, d) <- getService
      case mn of
        -- TODO Add RepTowage branch
        "towage" -> return Towage
        "rent"   -> return Rent
        "tech"   -> do
                techType <- dataField1 "techType" d
                case techType of
                  "charge"    -> return Charge
                  "condition" -> return Condition
                  "starter"   -> return Starter
                  _           -> exportError $ UnknownTechType techType
        _        -> exportError $ UnknownService mn

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
              return $ BS.append "G" $ padRight 2 '0' $ B8.pack $ show $ capRentDays d
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
                    -- Fetch contractor code for selected contractor
                    sPid <- dataField1 "contractor_partnerId" d
                    cp <- getCarmaPort
                    case B8.readInt sPid of
                      Just (pid, _) ->
                          do
                            pCode <- dataField0 "code" <$>
                                     (liftIO $ readInstance cp "partner" pid)
                            case mn of
                              "rent" ->
                                  do
                                    let pName = dataField0 "contractor_partner" d
                                        vin   = dataField0 "vinRent" d
                                        carCl = dataField0 "carClass" d
                                    return [oNum, pCode, pName, vin, carCl]
                              _      -> return [oNum, pCode]
                      Nothing -> exportError (UnreadableContractorId sPid) >>
                                 return []
        return $ BS.intercalate " " fields


getService :: ServiceExport Service
getService = ask


-- | Perform export action using the provided case and services data
-- and export options. If no errors occured, then return action result
-- and final state of export monad.
runExport :: CaseExport a
          -> Int
          -- ^ Initial value for @SEP@ line counter.
          -> ExportData
          -- ^ Case and all of its services.
          -> Int
          -- ^ CaRMa port.
          -> D.Dict
          -- ^ Wazzup dictionary.
          -> IO (Either ExportError (a, ExportState))
runExport act sepStart input cp wz = do
    res <- runErrorT $
           runReaderT (runStateT act $ ExportState sepStart) $
           (input, ExportOptions cp wz)
    return res


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


-- | A version of 'dataField' which requires non-empty field value and terminates with 'EmptyField' otherwise.
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
               -- ^ Name of field with first date. @callDate@ must exceed this.
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
    (Just t1, Just t, Just t2) -> t1 <= t && t > t2
    _ -> False


-- | Check if servicing contract is in effect.
onService :: ExportMonad m => m Bool
onService = callDateWithin "car_serviceStart" "car_serviceStop"


-- | Check if warranty is in effect.
onWarranty :: ExportMonad m => m Bool
onWarranty = callDateWithin "car_warrantyStart" "car_warrantyStop"


type ExportField = ExportMonad m => m BS.ByteString


cnst :: ExportMonad m => a -> m a
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
  putState $ s{counter = counter s + 1}
  return $ padRight 6 '0' $ B8.pack $ show $ counter s


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
spaces n = return $ B8.replicate n ' '


accordField :: ExportField
accordField = padRight 6 ' ' <$> caseField0 "accord"


nhmoField :: ExportField
nhmoField = spaces 5


somprField :: ExportField
somprField = spaces 10


fillerField :: ExportField
fillerField = spaces 5


commentPad :: BS.ByteString -> BS.ByteString
commentPad = padLeft 72 ' '


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
    , newline
    ]


comm1Part :: ExportPart
comm1Part =
    [ cnst "3"
    , comm1Field
    , newline
    ]


comm2Part :: ExportPart
comm2Part =
    [ cnst "4"
    , comm2Field
    , newline
    ]

comm3Part :: ExportPart
comm3Part =
    [ cnst "6"
    , comm3Field
    , newline
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


-- | Form a full entry for the case and its services.
sagaiFullExport :: CaseExport BS.ByteString
sagaiFullExport = do
  caseOut <- sagaiExport
  servs <- filter exportable <$> getAllServices
  servsOut <- BS.concat <$> mapM (\s -> runReaderT sagaiExport s) servs
  return $ BS.concat [caseOut, servsOut]
