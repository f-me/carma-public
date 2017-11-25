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

  # complex types
  history   : new CaseHistoryList

DiagTreeShowState = Record
  cases: Map() # Map<number (case id), CaseItem>

reducerMap =
  "#{actions.getCaseHistoryRequest}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem = new CaseItem id: payload.get "caseId") ->
        caseItem.merge
          isLoading : true
          isFailed  : false
          history   : new CaseHistoryList

  "#{actions.getCaseHistorySuccess}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading : false
        isFailed  : false
        history   : payload.get "history"

  "#{actions.getCaseHistoryFailure}": (state, {payload}) ->
    state.updateIn ["cases", payload.get "caseId"],
      (caseItem) -> caseItem.merge
        isLoading : false
        isFailed  : true
        history   : new CaseHistoryList

module.exports = handleActions reducerMap, new DiagTreeShowState
