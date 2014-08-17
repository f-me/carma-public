{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

{-|

Type system of Backoffice DSL.

-}

module Carma.Backoffice.DSL.Types
    (
      ActionAssignment
    , Eff
    , Outcome
    , Trigger

      -- * Haskell interface for Backoffice DSL
    , HaskellType
    , PreContextAccess(..)
    )

where

import           Prelude hiding (const)

import           Data.Functor
import           Data.Dynamic
import           Data.Text

import           Data.Map
import           Data.Model
import           Data.Model.Patch

import qualified Carma.Model.Action as CarmaAction
import           Carma.Model.Case as Case
import           Carma.Model.Role as Role
import           Carma.Model.Service as Service
import           Carma.Model.Usermeta (Usermeta)

import           Control.Monad.Free
import           Trigger.Dsl


data ActionAssignment


-- | An effect induced by changes in model @m@.
data Eff m


-- | An outcome induced by changes in model @m@.
data Outcome m


data Trigger


-- | Haskell embeddings of DSL types.
--
-- Constraints on HaskellType-projections of DSL types included in
-- Backoffice method signatures define relations between type systems
-- of our DSL and the meta-language.
type family HaskellType t where
  HaskellType Trigger = Map (Text, Text) [Dynamic]
  HaskellType (Maybe v) = Maybe (HaskellType v)
  HaskellType ActionAssignment = (Maybe (IdentI Usermeta), IdentI Role)
  HaskellType (Outcome m) = Free (Dsl m) ()
  HaskellType (Eff m) = Free (Dsl m) ()
  HaskellType t = t


-- | Helper class to provide back office context depending on trigger
-- models (@m@ in @Dsl m@).
class PreContextAccess m where
    getKase    :: Free (Dsl m) (Patch Case)
    getService :: Free (Dsl m) (Maybe (Patch Service))


instance PreContextAccess Case where
    getKase    = dbRead =<< getIdent
    getService = return Nothing


instance PreContextAccess Service where
    getService = Just <$> (dbRead =<< getIdent)


instance PreContextAccess CarmaAction.Action where
    getKase = do
      i <- getIdent
      p <- dbRead i
      dbRead $ get' p CarmaAction.caseId

    getService = do
      i <- getIdent
      p <- dbRead i
      let sId   = get' p CarmaAction.serviceId
          sType = get' p CarmaAction.serviceType
      case (sId, sType) of
        (Just sId', Just sType') -> Just <$> getSrv sId' sType'
        _                        -> return Nothing
