{Immutable: {Record, List}} = require "carma/vendor"

{makeActions, catchFailure, fetchGet} =
  require "carma/neoComponents/store/utils"

{CaseHistoryList} = require "./models"

actions = makeActions __dirname,
  getCaseHistoryRequest:
    Payload: Record
      caseId: 0

    handler: ({payload}, dispatch) ->
      caseId = payload.get "caseId"
      fetchGet "/diag/history/#{caseId}"
        # constructing immutable data structure
        .then (history) -> CaseHistoryList.fromPlain history
        # dispatching successs action
        .then (history) ->
          success = actions.getCaseHistorySuccess
          dispatch success new success.Payload {caseId, history}
        .catch (error) ->
          failure = actions.getCaseHistoryFailure
          dispatch failure new failure.Payload {caseId, error}
          throw error # throw exception forward

  getCaseHistorySuccess:
    Payload: Record
      caseId: 0
      history: List()

  getCaseHistoryFailure:
    Payload: Record
      caseId: 0
      error: null

module.exports = actions
