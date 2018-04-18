###
See "diag-tree-show" component descriptions for details.
###

{
  Immutable: {Record, Map}
  ReduxActions: {handleActions}
} = require "carma/vendor"

{CaseHistoryList} = require "./models"
actions = require "./actions"


CaseItem = Record
  # scalar types
  id        : 0
  isLoading : false
  isFailed  : false

  isGetCaseHistoryFailed : false
  isRepeatQuestionFailed : false
  isCreateServiceFailed  : false
  isAnswerFailed         : false

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


answerReducers =
  "#{actions.answerRequest}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading      : true
        isFailed       : false
        isAnswerFailed : false

  "#{actions.answerSuccess}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading      : false
        isFailed       : false
        isAnswerFailed : false

  "#{actions.answerFailure}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading      : false
        isFailed       : true
        isAnswerFailed : true


createServiceReducers =
  "#{actions.answerRequest}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading             : true
        isFailed              : false
        isCreateServiceFailed : false

  "#{actions.answerSuccess}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading             : false
        isFailed              : false
        isCreateServiceFailed : false

  "#{actions.answerFailure}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading             : false
        isFailed              : true
        isCreateServiceFailed : true


reducerMap = Object.assign {},
  getCaseHistoryReducers,
  repeatQuestionReducers,
  createServiceReducers,
  answerReducers

module.exports = handleActions reducerMap, new DiagTreeShowState
