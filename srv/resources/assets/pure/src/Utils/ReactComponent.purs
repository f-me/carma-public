-- Convention about passing `appContext` property:
--   1. Every component must require to receive `appContext` prop;
--   2. Every component must pass `appContext` prop to every child component.
-- This will help to connect to the store at any level of components tree
-- without changing anything else but that component.
--
-- Convention about declaring components:
--   1. Give a name to a component (e.g. `todoList`);
--   2. Declare two names:
--      1. `todoListRender` - which is stateless component that maps props to
--         `ReactElement` (see `createClassStateless`) or a usual class if you
--         need internal encapsulated state or to set hooks on lifecycle
--         (see `createClass` or `createClassStatelessWithSpec`);
--      2. `todoList` - which is either just an alias to `todoListRender`
--         (`todoList = todoListRender`) or connection to the store wrapper
--         (see `StoreConnect`).
--   3. Export explicitly `todoList` (you may also need to export
--      `todoListRender` to use in tests or any specific purposes but usually
--      you will use only `todoList`).
-- This will help to connect any component to the store just by rewriting
-- `todoList` definition.

module Utils.ReactComponent
     ( RequiredProps
     , createClassStatelessWithSpec
     , createClassStatelessWithSpec'
     , createClassStatelessWithName
     , createClassStatelessWithName'
     ) where

import Prelude

import React
     ( ReactElement, ReactClass, ReactSpec, ReactProps
     , class ReactRender, createClass, spec, getProps, getChildren
     )

import App.Store (AppContext)


type RequiredProps props =
  { appContext :: AppContext | props }

type SpecMiddleware props render eff
   = ReactSpec (RequiredProps props) Unit render (props :: ReactProps | eff)
  -> ReactSpec (RequiredProps props) Unit render (props :: ReactProps | eff)


-- Helper that works kinda like `createClassStateless`
-- but provides ability to customize `ReactSpec`.
-- This could be useful for example to set `shouldComponentUpdate`.
createClassStatelessWithSpec
  :: forall props render eff
   . ReactRender render
  => SpecMiddleware props render eff
  -> (RequiredProps props -> render)
  -> ReactClass (RequiredProps props)

createClassStatelessWithSpec specMiddleware pureRender =
  createClass $ specMiddleware $ spec unit $ getProps >>> map pureRender


-- See `createClassStatelessWithSpec`, this one just can deal with children,
-- as `createClassStateless'` can comparing with `createClassStateless`.
createClassStatelessWithSpec'
  :: forall props render eff
   . ReactRender render
  => SpecMiddleware props render eff
  -> (RequiredProps props -> Array ReactElement -> render)
  -> ReactClass (RequiredProps props)

createClassStatelessWithSpec' specMiddleware pureRender =
  createClass $ specMiddleware $ spec unit $ \this -> do
    props    <- getProps    this
    children <- getChildren this
    pure $ pureRender props children


-- Helper to just set display-name to a component.
createClassStatelessWithName
  :: forall props render
   . ReactRender render
  => String
  -> (RequiredProps props -> render)
  -> ReactClass (RequiredProps props)

createClassStatelessWithName name =
  createClassStatelessWithSpec _ { displayName = name }


-- See `createClassStatelessWithName`, this one just can deal with children,
-- as `createClassStateless'` can comparing with `createClassStateless`.
createClassStatelessWithName'
  :: forall props render
   . ReactRender render
  => String
  -> (RequiredProps props -> Array ReactElement -> render)
  -> ReactClass (RequiredProps props)

createClassStatelessWithName' name =
  createClassStatelessWithSpec' _ { displayName = name }
