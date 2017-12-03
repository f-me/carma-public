{
  Immutable: {Record}
  ReduxActions: {handleActions}
} = require "carma/vendor"

actions = require "./actions"
{SmsTemplateList} = require "./models"

TemplatesState = Record
  list      : new SmsTemplateList
  isLoading : false
  isLoaded  : false
  isFailed  : false

SmsFormState = Record
  isShown      : false
  isProcessing : false
  isFailed     : false

  phone        : ""
  caseId       : ""
  caseCity     : ""
  caseAddress  : ""

  templates    : new TemplatesState


formFlow =

  "#{actions.showSmsForm}":
    (state, {payload}) -> state.set("isShown", true).merge payload

  "#{actions.closeSmsForm}": (state) -> state.merge
    isProcessing : false
    isFailed     : false
    isShown      : false


sendSmsFlow =

  "#{actions.sendSmsFormRequest}": (state) -> state.merge
    isProcessing : true
    isFailed     : false

  "#{actions.sendSmsFormSuccess}": (state) -> state.merge
    isProcessing : false
    isFailed     : false
    isShown      : false

  "#{actions.sendSmsFormFailure}": (state) -> state.merge
    isProcessing : false
    isFailed     : true


loadSmsTemplatesFlow =

  "#{actions.loadSmsTemplatesRequest}": (state) ->
    state.mergeIn ["templates"],
      isLoading : true
      isLoaded  : false
      isFailed  : false

  "#{actions.loadSmsTemplatesSuccess}": (state, {payload}) ->
    state.mergeIn ["templates"],
      isLoading : false
      isLoaded  : true
      list      : payload.get "smsTemplates"

  "#{actions.loadSmsTemplatesFailure}": (state) ->
    state.mergeIn ["templates"],
      isLoading : false
      isFailed  : true


reducerMap = Object.assign {},
  formFlow,
  sendSmsFlow,
  loadSmsTemplatesFlow

module.exports = handleActions reducerMap, new SmsFormState
