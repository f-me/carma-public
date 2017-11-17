{ReduxActions: {handleActions}} = require "carma/vendor"
{forkObj} = require "carma/neoComponents/store/utils"
actions = require "./actions"

defaultState =
  isShown:      false
  isProcessing: false
  isFailed:     false

  phone:        ""
  caseId:       ""
  caseCity:     ""
  caseAddress:  ""

reducerMap =
  "#{actions.showSmsForm}":
    (state, {payload: {phone, caseId, caseCity, caseAddress}}) ->
      forkObj state, {isShown: true, phone, caseId, caseCity, caseAddress}

  "#{actions.closeSmsForm}": (state) -> forkObj state,
    isProcessing: false
    isFailed:     false
    isShown:      false

  "#{actions.sendSmsFormRequest}": (state) -> forkObj state,
    isProcessing: true
    isFailed:     false

  "#{actions.sendSmsFormSuccess}": (state) -> forkObj state,
    isProcessing: false
    isFailed:     false
    isShown:      false

  "#{actions.sendSmsFormFailure}": (state) -> forkObj state,
    isProcessing: false
    isFailed:     true

module.exports = handleActions reducerMap, defaultState
