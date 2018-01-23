{
  Redux: {createStore, applyMiddleware}
  ReduxThunk
  ReduxImmutable: {combineReducers}
} = require "carma/vendor"

reducers = combineReducers
  smsForm      : require "./smsForm/reducers"
  diagTree     : require "./diagTree/reducers"
  avarcomTasks : require "./avarcomTasks/reducers"
  navbar       : require "./navbar/reducers"

store = createStore reducers, applyMiddleware ReduxThunk

module.exports = {store}
