{ReduxImmutable: {combineReducers}} = require "carma/vendor"

reducersMap =
  show: require "./show/reducers"

module.exports = combineReducers reducersMap
