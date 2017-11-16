{ReduxActions: {handleActions}} = require "carma/vendor"
{showSmsForm, closeSmsForm} = require "./actions"

defaultState =
  isShown:     false
  phone:       ""
  caseId:      ""
  caseCity:    ""
  caseAddress: ""

reducerMap =
  "#{showSmsForm}":
    (state, {payload: {phone, caseId, caseCity, caseAddress}}) ->
      Object.assign {}, state,
        {isShown: true, phone, caseId, caseCity, caseAddress}

  "#{closeSmsForm}": (state) -> Object.assign {}, state, isShown: false

module.exports = handleActions reducerMap, defaultState
