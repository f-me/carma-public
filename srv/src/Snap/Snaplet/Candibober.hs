{-# LANGUAGE OverloadedStrings #-}

-- | Candibober takes dataset and checks if named conditions hold for
-- them.

module Snap.Snaplet.Candibober
    (Candibober,
     candiboberInit
    )

where

import Control.Applicative

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB (readFile)

import Data.Lens.Common

import qualified Data.Map as M

import Snap.Snaplet
import Snap.Snaplet.Redson.Snapless.Metamodel


type SlotName = B.Bytestring


-- | Datasets provided to Candibober may have several named slots,
-- each containing a commit (think several model instances).
type Dataset = M.Map SlotName Commit


-- | Checker function for condition.
type Checker = Dataset -> CheckResult


-- | If condition check explicitly failed or succeeded, Just Bool is
-- returned. If no data was provided, then checkers return Nothing.
type CheckResult = Maybe Bool


data Condition = Condition { cType :: B.Bytestring,
                             check :: Checker
                           }


type TargetMap = M.Map TargetName [Condition]


data Candibober = Candibober { _targets :: TargetMap }

makeLens ''Candibober


------------------------------------------------------------------------------
-- | Parse target definitions into 'TargetMap'.
loadTargets :: FilePath
            -- ^ Targets definition file
            -> IO (Maybe TargetMap)
loadTargets filename = A.decode <$> LB.readFile filename


------------------------------------------------------------------------------
-- | Make 'Candibober' snaplet.
candiboberInit :: SnapletInit b Candibober
candiboberInit =
    makeSnaplet "candibober" "Conditions checker snaplet." Nothing $ do
      cfg <- getSnapletUserConfig
      tFile <- liftIO $
               lookupDefault "resources/targets.json"
                             cfg "targets-file"

      tMap <- loadTargets tFile
      return $ Candibober tMap
