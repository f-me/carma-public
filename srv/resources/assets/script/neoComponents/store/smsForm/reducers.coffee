{
  Immutable: {Record}
  ReduxActions: {handleActions}
} = require "carma/vendor"

actions = require "./actions"

SmsFormState = Record
  isShown:      false
  isProcessing: false
  isFailed:     false

  phone:        ""
  caseId:       ""
  caseCity:     ""
  caseAddress:  ""

reducerMap =
  "#{actions.showSmsForm}":
    (state, {payload}) -> state.set("isShown", true).merge(payload)

  "#{actions.closeSmsForm}": (state) -> state.merge
    isProcessing: false
    isFailed:     false
    isShown:      false

  "#{actions.sendSmsFormRequest}": (state) -> state.merge
    isProcessing: true
    isFailed:     false

  "#{actions.sendSmsFormSuccess}": (state) -> state.merge
    isProcessing: false
    isFailed:     false
    isShown:      false

  "#{actions.sendSmsFormFailure}": (state) -> state.merge
    isProcessing: false
    isFailed:     true

module.exports = handleActions reducerMap, new SmsFormState
