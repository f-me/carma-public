{
  Immutable: {Record}
  ReduxActions: {handleActions}
} = require "carma/vendor"

{AvarcomTasksList} = require "./models"
actions = require "./actions"

AvarcomTasksState = Record
  # scalar types
  isLoaded  : false
  isLoading : false
  isFailed  : false

  # complex types
  tasks: new AvarcomTasksList

reducerMap =
  "#{actions.getAvarcomTasksRequest}": (state) -> state.merge
    isLoaded  : false
    isLoading : true
    isFailed  : false
    tasks     : new AvarcomTasksList

  "#{actions.getAvarcomTasksSuccess}": (state, {payload}) -> state.merge
    isLoaded  : true
    isLoading : false
    isFailed  : false
    tasks     : payload.get "avarcomTasks"

  "#{actions.getAvarcomTasksFailure}": (state) -> state.merge
    isLoaded  : false
    isLoading : false
    isFailed  : true

module.exports = handleActions reducerMap, new AvarcomTasksState
