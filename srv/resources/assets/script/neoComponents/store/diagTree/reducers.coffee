{
  Immutable: {Record}
  ReduxActions: {handleActions}
} = require "carma/vendor"

actions = require "./actions"

DiagTreeState = Record
  foo: 1

reducerMap =
  "#{actions.fooAction}": (state) -> state.update "foo", ((x) -> x + 1)

module.exports = handleActions reducerMap, new DiagTreeState
