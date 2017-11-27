{
  Immutable: {Record, Map, List}
  ReduxActions: {handleActions}
} = require "carma/vendor"

{CaseHistoryList} = require "./models"
actions = require "./actions"


CaseItem = Record
  # scalar types
  id        : 0
  isLoading : false
  isFailed  : false

  isGetCaseHistoryFailed: false
  isRepeatQuestionFailed: false

  # complex types
  history: new CaseHistoryList


DiagTreeShowState = Record
  cases: Map() # Map<number (case id), CaseItem>


getCaseHistoryReducers =
  "#{actions.getCaseHistoryRequest}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem = new CaseItem id: payload.get "caseId") ->
        caseItem.merge
          history                : new CaseHistoryList
          isLoading              : true
          isFailed               : false
          isGetCaseHistoryFailed : false

  "#{actions.getCaseHistorySuccess}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.withMutations (x) ->
        x.set "isLoading", false if payload.get "isOnlyAction"
        x.merge
          history                : payload.get "history"
          isFailed               : false
          isGetCaseHistoryFailed : false

  "#{actions.getCaseHistoryFailure}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.withMutations (x) ->
        x.set "isLoading", false if payload.get "isOnlyAction"
        x.merge
          history                : new CaseHistoryList
          isFailed               : true
          isGetCaseHistoryFailed : true


repeatQuestionReducers =
  # case branch supposed to be initiated at this moment
  "#{actions.repeatQuestionRequest}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading              : true
        isFailed               : false
        isRepeatQuestionFailed : false

  "#{actions.repeatQuestionSuccess}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading              : false
        isFailed               : false
        isRepeatQuestionFailed : false

  "#{actions.repeatQuestionFailure}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading              : false
        isFailed               : true
        isRepeatQuestionFailed : true


reducerMap = Object.assign {}, getCaseHistoryReducers, repeatQuestionReducers
module.exports = handleActions reducerMap, new DiagTreeShowState
