{-# LANGUAGE ExplicitNamespaces, KindSignatures, ConstraintKinds #-}

module Carma.EraGlonass.Types.Helpers.Proof
     ( type ProofThatTheTypeIsComplete
     , proofThatTheTypeIsComplete
     ) where

import           GHC.Generics (type Generic)

import           Data.Proxy
import           Data.Aeson (type FromJSON, type ToJSON)
import           Data.Swagger (type ToSchema)


-- | It proofs that request type (and response type related to that request)
--   has all the required instances.
type ProofThatTheTypeIsComplete (t :: *) =
   ( Eq t
   , Show t
   , Generic t
   , FromJSON t
   , ToJSON t
   , ToSchema t
   )


proofThatTheTypeIsComplete :: ProofThatTheTypeIsComplete t => Proxy t -> ()
proofThatTheTypeIsComplete Proxy = ()
