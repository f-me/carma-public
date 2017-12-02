{
  Redux: {createStore, applyMiddleware}
  ReduxThunk
  ReduxImmutable: {combineReducers}
} = require "carma/vendor"

reducers = combineReducers
  smsForm      : require "./smsForm/reducers"
  diagTree     : require "./diagTree/reducers"
  avarcomTasks : require "./avarcomTasks/reducers"

store = createStore reducers, applyMiddleware ReduxThunk

module.exports = {store}
