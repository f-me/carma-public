module Utils
     ( createReactClass
     ) where

import Prelude

import React (ReactSpec, ReactThis, ReactClass, createClass) as React
import Thermite (Spec, EventHandler, createReactSpec) as T


createReactClass
  :: forall eff state props action
   . T.Spec eff state props action
  -> state
  -> (
       React.ReactSpec props state eff
       -> (React.ReactThis props state -> action -> T.EventHandler)
       -> React.ReactSpec props state eff
     )
  -> React.ReactClass props
createReactClass spec initialStatae f =
  React.createClass $ f tSpec.spec tSpec.dispatcher
  where tSpec = T.createReactSpec spec initialStatae
