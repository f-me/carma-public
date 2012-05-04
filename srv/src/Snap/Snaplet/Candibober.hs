{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Candibober takes dataset and checks if named conditions hold for
-- them.

module Snap.Snaplet.Candibober
    (Candibober,
     candiboberInit
    )

where

import Control.Applicative
import Data.Maybe

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Error

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

import Data.Configurator

import Data.Lens.Template

import qualified Data.Map as M

import           Snap.Snaplet.RedisDB
import qualified Database.Redis as R

import Snap.Core
import Snap.Snaplet
import Snap (gets)

import qualified Snap.Snaplet.Redson.Snapless.CRUD as CRUD

import Snap.Snaplet.Candibober.Types

import Snap.Snaplet.Candibober.Date
import Snap.Snaplet.Candibober.Number
import Snap.Snaplet.Candibober.String


------------------------------------------------------------------------------
-- | Map JSON types to checker argument types, allowing for further
-- pattern-matching in arg processing combinators
instance FromJSON CheckerArgs where
    parseJSON s@(String _) = Single <$> (parseJSON s)
    parseJSON v@(Array _) = Many <$> (parseJSON v)
    parseJSON _ = error "Could not parse check arguments"


------------------------------------------------------------------------------
-- | Named checks built from combinators.
--
-- This is where all known checks are defined. 'FreeChecker's are may
-- be built using combinators of form @f . g@, where @g@ parses
-- CheckerArgs into type consumed by @f@, which produces a 'Checker'.
--
-- CheckerArgs of @g@ are parsed from JSON.
--
-- Kleisli composition here means proper error-handling on every step
-- of 'FreeChecker' monadic chain.
checkMap :: M.Map B.ByteString (FreeChecker A.Parser)
checkMap = 
    M.fromList 
         [ ("sellLess", 
            compareDate "case" "car_sellDate" LT <=< yearsAgo <=< readInteger)
         , ("sellAfter",
            compareDate "case" "car_sellDate" GT <=< readDate)
         , ("checkupLess",
            compareDate "case" "car_checkupDate" LT <=< yearsAgo <=< readInteger)
         , ("mileageLess",
            compareNumber "case" "car_checkupMileage" LT <=< readInteger)
         , ("daysPassedSinceReport",
            compareDate "service" "car_checkupDate" GT <=< daysAgo <=< readInteger)
         , ("modelInList",
            fieldInList "case" "car_model" <=< readManyStrings)
         , ("ruamcEvac",
            fieldContains "case" "services" "towage" <<< readNone)
         , ("notVandal",
            fieldEquals "case" "notVandal" "1" <<< readNone)
         , ("notAccident",
            fieldEquals "case" "notAccident" "1" <<< readNone)
         , ("notUsedService",
            (inverseChecker . fieldContains "case" "services") <=< readSingleString)
         ]


type ConditionName = B.ByteString


data Condition = Condition { cType :: ConditionName
                           , checker :: Checker
                           }


instance FromJSON Condition where
    parseJSON (Object v) = do
      checkType <- v .: "type"
      case M.lookup checkType checkMap of
        Just freeChecker -> do
            -- | Apply as much of FreeChecker chain as possible.
            --
            -- If "args" is not present in target description, feed
            -- NoArgs.
            chain <- runErrorT =<< freeChecker <$>
                     fromMaybe NoArgs <$> (v .:? "args")
            case chain of
              Left e -> error $ "Could not build checker: " ++ (show e)
              Right checker -> return $ Condition checkType checker
        Nothing -> error $ "Unknown check type " ++ (B.unpack checkType)
    parseJSON _ = error "Could not parse condition"


type TargetName = B.ByteString


------------------------------------------------------------------------------
-- | Each target is a named list of conditions.
type TargetMap = M.Map TargetName [Condition]


------------------------------------------------------------------------------
-- | Candibober snaplet type.
data Candibober = Candibober { _targets :: TargetMap
                             , _database :: Snaplet RedisDB
                             }

makeLens ''Candibober

jsonToDataset :: LB.ByteString -> Maybe Dataset
jsonToDataset s = A.decode s

-- | Generate redis key from model name and id
getKey m =
  let name = fromJust $ M.lookup "model" m
      id   = fromJust $ M.lookup "id"    m
  in CRUD.instanceKey name id

-- | get full model, I think here should be Left r case
updateModel v = runRedisDB database $ do
                  Right r <- R.hgetall $ getKey v
                  return $ M.fromList r

-- | Recursively build dataset from redis based on recieved spec
updateDataset dataset = upd (M.toList dataset) M.empty
    where
      upd [] d2 = return d2
      upd d1 d2 = do
        let (k, v) = head d1
        n <- updateModel v
        let m = M.insert k n d2
        upd (tail d1) m

data CheckedConditions = CCond { true, false, nothing :: [ConditionName] }
$(deriveToJSON id ''CheckedConditions)

-- | check conditions on dataset and divide them it 3 groups
check conditions ds = check' conditions $ CCond [] [] []
    where
      check' []     cc = return cc
      check' (c:cs) cc = do
        r <- (checker c) ds
        case r of
          Nothing    -> check' cs $ cc { nothing = (cType c) : (nothing cc) }
          Just True  -> check' cs $ cc { true    = (cType c) : (true cc   ) }
          Just False -> check' cs $ cc { false   = (cType c) : (false cc  ) }

------------------------------------------------------------------------------
-- | Read target name from @target@ request parameter and dataset spec
-- from request body, respond with list of passed and failed
-- conditions.
--
-- Dataset spec is a hash:
--
-- @
-- { "slot1" : { "model": "foo", "id": "2231"},
--   "slot2" : { "model": "bar", "id": "69"}
-- }
-- @
--
-- Actual dataset is built by fetching instances of named models with
-- that ids into respective slots of dataset. If instance is not
-- found, 404 is raised.
doCheck :: Handler b Candibober ()
doCheck = do
    modifyResponse $ setContentType "application/json"
    targetName <- getParam "target"
    targets    <- gets _targets
    target     <- return $ targetName >>= \x -> M.lookup x targets
    dataset    <- jsonToDataset <$> readRequestBody 65535
    u <- updateDataset $ fromJust dataset
    r <- liftIO $ check (fromJust target) u
    writeLBS $ A.encode r

------------------------------------------------------------------------------
-- | Parse target definitions into 'TargetMap'.
loadTargets :: FilePath
            -- ^ Targets definition file
            -> IO TargetMap
loadTargets filename = do
    res <- A.decode <$> LB.readFile filename
    case res of
      Just t -> return t
      Nothing -> error $ "Could not parse targets file " ++ filename


------------------------------------------------------------------------------
-- | Candibober routes.
routes :: [(B.ByteString, Handler b Candibober ())]
routes = [ ("/check/:target", method POST $ doCheck) ]

------------------------------------------------------------------------------
-- | Make 'Candibober' snaplet.
candiboberInit :: SnapletInit b Candibober
candiboberInit =
    makeSnaplet "candibober" "Conditions checker snaplet." Nothing $ do
      cfg <- getSnapletUserConfig
      tFile <- liftIO $
               lookupDefault "resources/targets.json"
                             cfg "targets-file"
      r <- nestSnaplet "db" database $ redisDBInit R.defaultConnectInfo
      tMap <- liftIO $ loadTargets tFile
      addRoutes routes
      return $ Candibober tMap r
