{Redux: {createStore, combineReducers}} = require "carma/vendor"

store = createStore combineReducers
  smsForm: require "./smsForm/reducers"

module.exports = {store}
