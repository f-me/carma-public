{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

{-|

Type system of Backoffice DSL.

-}

module Carma.Backoffice.DSL.Types
    (
      Eff
    , Outcome
    , Trigger
    , MailType(..)

      -- * Haskell interface for Backoffice DSL
    , HaskellType
    , PreContextAccess(..)
    , SvcAccess(..)
    )

where

import           Prelude hiding (const)

import           Data.Functor
import           Data.Dynamic
import           Data.Text

import           Data.Map
import           Data.Model.Patch
import           Data.Model.Types

import qualified Carma.Model.Action as CarmaAction
import           Carma.Model.Case as Case
import           Carma.Model.Service as Service

import           Control.Monad.Free
import           Trigger.Dsl


-- | An effect induced by changes in model @m@.
--
-- The inducing model defines available context.
data Eff m


-- | An outcome induced by changes in model @m@.
--
-- This is similar to 'Eff', but should be viewed as a finishing
-- action in a chain of effects.
data Outcome m


data Trigger


-- | Mail type/destination.
data MailType = Dealer | PSA | Genser


-- | Haskell embeddings of DSL types.
--
-- Constraints on HaskellType-projections of DSL types included in
-- Backoffice method signatures define relations between type systems
-- of our DSL and the meta-language.
type family HaskellType t where
  HaskellType Trigger = Map (Text, Text) [Dynamic]
  HaskellType (Maybe v) = Maybe (HaskellType v)
  HaskellType (Outcome m) = Free (Dsl m) ()
  HaskellType (Eff m) = Free (Dsl m) ()
  HaskellType t = t


-- | Provides back office context depending on trigger models (@m@ in
-- @Dsl m@).
class PreContextAccess m where
  getKase    :: Free (Dsl m) (Patch Case)
  getService :: Free (Dsl m) (Maybe (Patch Service))
  getAction  :: Free (Dsl m) (Maybe (Patch CarmaAction.Action))


instance PreContextAccess Case where
  getKase    = dbRead =<< getIdent
  getService = return Nothing
  getAction  = return Nothing


instance PreContextAccess Service where
  getKase =
    dbRead =<< (`get'` Service.parentId) <$> (dbRead =<< getIdent)

  getService = Just <$> (dbRead =<< getIdent)
  getAction  = return Nothing


instance PreContextAccess CarmaAction.Action where
  getKase =
    dbRead =<< (`get'` CarmaAction.caseId) <$> (dbRead =<< getIdent)

  getService = do
    p <- dbRead =<< getIdent
    case get' p CarmaAction.serviceId of
      Just i' -> Just <$> dbRead i'
      Nothing -> return Nothing

  getAction = Just <$> (dbRead =<< getIdent)


-- | Provides write access to the service, possibly updating the
-- patch.
--
-- This is a workaround until 'dbUpdate' is fixed to include its
-- updates in the patch.
class SvcAccess m where
  -- | Set a field in the service to a new value.
  setService :: FieldI t n d =>
                (IdentI Service)
             -> (Service -> F t n d)
             -> t
             -> Free (Dsl m) ()


instance SvcAccess Service where
  setService i acc val = do
    modifyPatch $ put acc val
    void $ dbUpdate i $ put acc val Data.Model.Patch.empty


instance SvcAccess CarmaAction.Action where
  setService i acc val =
    void $ dbUpdate i $ put acc val Data.Model.Patch.empty
