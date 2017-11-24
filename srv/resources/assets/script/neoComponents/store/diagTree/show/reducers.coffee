{
  Immutable: {Record, Map, List}
  ReduxActions: {handleActions}
} = require "carma/vendor"

actions = require "./actions"

CaseItem = Record
  # scalar types
  id           : 0
  isProcessing : false
  isFailed     : false

  # complex types
  history      : List() # for model see "./actions" module

DiagTreeShowState = Record
  cases: Map() # Map<number (case id), CaseItem>

reducerMap =
  "#{actions.getCaseHistoryRequest}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem = new CaseItem id: payload.get "caseId") ->
        caseItem.merge
          isProcessing : true
          isFailed     : false
          history      : List()

  "#{actions.getCaseHistorySuccess}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isProcessing : false
        isFailed     : false
        history      : payload.get "history"

  "#{actions.getCaseHistoryFailure}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isProcessing : false
        isFailed     : true
        history      : List()

module.exports = handleActions reducerMap, new DiagTreeShowState
