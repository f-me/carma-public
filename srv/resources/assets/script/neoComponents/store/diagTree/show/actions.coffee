{Immutable: {Record, List}} = require "carma/vendor"
{makeActions, fetchGet, fetchPost} = require "carma/neoComponents/store/utils"
{CaseHistoryList} = require "./models"


getCaseHistoryFlow =

  getCaseHistoryRequest:
    Payload: Record
      caseId: 0

      # This action dispatched independently,
      # not as part of another action's flow
      # (to prevent unsetting `isLoading` flag after this one is done).
      isOnlyAction: true

    handler: ({payload}, dispatch) ->
      caseId = payload.get "caseId"
      isOnlyAction = payload.get "isOnlyAction"
      fetchGet "/diag/history/#{caseId}"
        # constructing immutable data structure
        .then (history) -> CaseHistoryList.fromPlain history
        # dispatching success action
        .then (history) ->
          success = actions.getCaseHistorySuccess
          dispatch success new success.Payload {caseId, history, isOnlyAction}
        .catch (error) ->
          failure = actions.getCaseHistoryFailure
          dispatch failure new failure.Payload {caseId, error, isOnlyAction}
          throw error # throw exception forward

  getCaseHistorySuccess:
    Payload: Record
      caseId: 0
      history: List()
      isOnlyAction: true

  getCaseHistoryFailure:
    Payload: Record
      caseId: 0
      error: null
      isOnlyAction: true


repeatQuestionFlow =

  repeatQuestionRequest:
    Payload: Record
      caseId: 0
      historyItemId: 0

    handler: ({payload}, dispatch) ->
      caseId = payload.get "caseId"
      historyItemId = payload.get "historyItemId"
      fetchPost "/diag/retry/#{historyItemId}"
        .then ->
          request = actions.getCaseHistoryRequest
          dispatch request new request.Payload {caseId}
        .then ->
          success = actions.repeatQuestionSuccess
          dispatch success new success.Payload {caseId, historyItemId}
        .catch (error) ->
          failure = actions.repeatQuestionFailure
          dispatch failure new failure.Payload {caseId, historyItemId, error}
          throw error

  repeatQuestionSuccess:
    Payload: Record
      caseId: 0
      historyItemId: 0

  repeatQuestionFailure:
    Payload: Record
      caseId: 0
      historyItemId: 0
      error: null


actions = makeActions __dirname,
  Object.assign {}, getCaseHistoryFlow, repeatQuestionFlow

module.exports = actions
