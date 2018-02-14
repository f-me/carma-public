module Utils.ReactComponent
     ( createClassStatelessWithSpec
     ) where

import Prelude

import React ( ReactElement, ReactClass, ReactSpec, ReactProps
             , createClass, spec, getProps
             )

import Utils.StoreConnect (StoreConnectEff)
import App.Store (AppContext)


type RequiredProps props eff =
  { appContext :: AppContext (StoreConnectEff eff) | props }

type SpecMiddleware props eff
   = ReactSpec (RequiredProps props eff) Unit (props :: ReactProps | eff)
  -> ReactSpec (RequiredProps props eff) Unit (props :: ReactProps | eff)


-- Helper that works kinda `createClassStateless`
-- but provides ability to customize `ReactSpec`.
-- This could be useful for example to set `shouldComponentUpdate`.
createClassStatelessWithSpec
  :: forall props eff
   . SpecMiddleware props eff
  -> (RequiredProps props eff -> ReactElement)
  -> ReactClass (RequiredProps props eff)

createClassStatelessWithSpec specMiddleware pureRender =
  createClass $ specMiddleware $ spec unit renderFn
  where renderFn = getProps >>> map pureRender
