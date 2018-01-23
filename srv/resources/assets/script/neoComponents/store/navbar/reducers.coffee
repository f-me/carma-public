{
  Immutable: {Record}
  ReduxActions: {handleActions}
} = require "carma/vendor"

actions = require "./actions"
{NavbarMenuList} = require "./models"

NavbarState = Record
  menu: new NavbarMenuList
  current: "" # `NavbarMenuItem.name`

  # How many elements is hidden to dropdown due to screen width exceeded
  hidden: 0

reducerMap =
  "#{actions.fillMenu}": (state, {payload}) ->
    state.set "menu", NavbarMenuList.fromPlain payload.get "plainData"

  "#{actions.setCurrent}": (state, {payload}) ->
    state.set "current", payload.get "name"

  "#{actions.hide}": (state, {payload}) ->
    return state if state.get("hidden") >= state.get("menu").size
    state.update "hidden", (x) -> x + payload.get "count"

  "#{actions.resetHidden}": (state) ->
    state.set "hidden", 0

module.exports = handleActions reducerMap, new NavbarState
