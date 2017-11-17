{
  Redux: {createStore, combineReducers, applyMiddleware}
  ReduxThunk
} = require "carma/vendor"

reducers = combineReducers
  smsForm: require "./smsForm/reducers"

store = createStore reducers, applyMiddleware ReduxThunk

module.exports = {store}
