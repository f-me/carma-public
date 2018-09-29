{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

{-|

Partial Contract model to experiment with Persistent.

Must be a subset of Carma.Model.Contract.

-}

module Carma.Model.Contract.Persistent

where

import Data.Time.Clock (UTCTime)
import Data.Text (Text)

import Database.Persist.TH


-- | Partially implemented @Contract@ persistent model.
--
-- TODO extend with other fields
share [mkPersist sqlSettings] [persistLowerCase|
Contract json sql=Contract
  ctime UTCTime
  isActive Bool sql=isactive
  name Text Maybe
  phone Text Maybe
  dixi Bool
|]
