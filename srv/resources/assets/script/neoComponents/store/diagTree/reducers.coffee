{ReduxImmutable: {combineReducers}} = require "carma/vendor"

reducersMap =
  show: require "./show/reducers"
  edit: require "./edit/reducers"

module.exports = combineReducers reducersMap
