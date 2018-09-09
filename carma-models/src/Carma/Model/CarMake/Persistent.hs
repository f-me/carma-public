{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.CarMake.Persistent where

import           Data.Text (Text)
import           Data.Typeable

import           Database.Persist.TH
import           Database.Persist.Sql (toSqlKey)


-- | @CarMake@ persistent model.
mkPersist sqlSettings [persistLowerCase|
CarMake sql=CarMake
  value    Text              sql=value
  label    Text              sql=label
  synonyms Text Vector Maybe sql=synonyms
  fdds     Text        Maybe sql=fdds

  deriving Typeable Show
|]


-- | @CarMake@ predefined IDs.
vw, sy :: CarMakeId
vw = toSqlKey 1
sy = toSqlKey 48
