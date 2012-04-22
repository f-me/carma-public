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

import Control.Monad.State hiding (put)

import Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB (readFile)

import Data.Configurator

import Data.Lens.Template

import qualified Data.Map as M

import Snap.Snaplet
import Snap.Snaplet.Candibober.Checks


------------------------------------------------------------------------------
-- | Map JSON types to checker argument types, allowing for further
-- pattern-matching in arg processing combinators
instance FromJSON CheckerArgs where
    parseJSON s@(String _) = Single <$> (parseJSON s)
    parseJSON v@(Array _) = Many <$> (parseJSON v)
    parseJSON _ = error "Could not parse check arguments"


------------------------------------------------------------------------------
-- | Checker which has not been fully bound to check parameters yet.
-- Applying it to arguments yields a checker.
type FreeChecker = CheckerArgs -> Checker


------------------------------------------------------------------------------
-- | Named checks built from combinators.
--
-- This is where all known checks are defined. 'FreeChecker's are may
-- be built using combinators of form @f . g@, where @g@ parses
-- CheckerArgs into type consumed by @f@, which produces a 'Checker'.
--
-- CheckerArgs of @g@ are parsed from JSON.
checkMap :: M.Map B.ByteString FreeChecker
checkMap = 
    M.fromList 
         [ ("sellLess", dateCheck "case" "car_sellDate" LT . yearsAgo)
         ]


type ConditionName = B.ByteString


data Condition = Condition { cType :: ConditionName
                           , checker :: Checker
                           }


instance FromJSON Condition where
    parseJSON (Object v) = do
      checkType <- v .: "type"
      case M.lookup checkType checkMap of
        Just freeChecker -> Condition "foo" <$> freeChecker <$> v .: "args"
        Nothing -> error $ "Unknown check type " ++ (B.unpack checkType)
    parseJSON _ = error "Could not parse condition"


type TargetName = B.ByteString


------------------------------------------------------------------------------
-- | Each target is a named list of conditions.
type TargetMap = M.Map TargetName [Condition]


------------------------------------------------------------------------------
-- | Candibober snaplet type.
data Candibober = Candibober { _targets :: TargetMap }

makeLens ''Candibober


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
-- | Make 'Candibober' snaplet.
candiboberInit :: SnapletInit b Candibober
candiboberInit =
    makeSnaplet "candibober" "Conditions checker snaplet." Nothing $ do
      cfg <- getSnapletUserConfig
      tFile <- liftIO $
               lookupDefault "resources/targets.json"
                             cfg "targets-file"

      tMap <- liftIO $ loadTargets tFile
      return $ Candibober tMap
