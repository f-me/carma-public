{Immutable: {Record, List}} = require "carma/vendor"

{makeActions, fetchGet, fetchPost, fetchPut} =
  require "carma/neoComponents/store/utils"

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
      {caseId, isOnlyAction} = payload.toJS()
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
      {caseId, historyItemId} = payload.toJS()
      fetchPost "/diag/retry/#{historyItemId}"
        .then ->
          request = actions.getCaseHistoryRequest
          dispatch request new request.Payload {caseId, isOnlyAction: false}
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


answerFlow =

  answerRequest:
    Payload: Record
      caseId: 0
      slideId: 0
      nextSlideId: 0
      answerIndex: 0

    handler: ({payload}, dispatch) ->
      {caseId, slideId, nextSlideId, answerIndex} = payload.toJS()
      data = answerIx: answerIndex
      opts = originalResponse: true
      fetchPut "/_/DiagHistory/#{slideId}", data, opts
        .then (response) ->
          if response.status isnt 200
            throw new Error "Server response status is not 200"
        .then -> fetchPost "/_/DiagHistory", {caseId, slideId: nextSlideId}
        .then ->
          request = actions.getCaseHistoryRequest
          dispatch request new request.Payload {caseId, isOnlyAction: false}
        .then ->
          success = actions.answerSuccess
          dispatch success new success.Payload \
            {caseId, slideId, nextSlideId, answerIndex}
        .catch (error) ->
          failure = actions.answerFailure
          dispatch failure new failure.Payload \
            {caseId, slideId, nextSlideId, answerIndex, error}
          throw error

  answerSuccess:
    Payload: Record
      caseId: 0
      slideId: 0
      nextSlideId: 0
      answerIndex: 0

  answerFailure:
    Payload: Record
      caseId: 0
      slideId: 0
      nextSlideId: 0
      answerIndex: 0
      error: null


createServiceFlow =

  createServiceRequest:
    Payload: Record
      caseId: 0
      serviceModelName: ""

    handler: ({payload}, dispatch) ->
      {caseId, serviceModelName} = payload.toJS()
      fetchPost "/_/#{serviceModelName}", parentId: caseId
        .then ->
          success = actions.createServiceSuccess
          dispatch success new success.Payload {caseId, serviceModelName}
        .catch (error) ->
          failure = actions.createServiceFailure
          dispatch failure new failure.Payload {caseId, serviceModelName, error}
          throw error

  createServiceSuccess:
    Payload: Record
      caseId: 0
      serviceModelName: ""

  createServiceFailure:
    Payload: Record
      caseId: 0
      serviceModelName: ""
      error: null


actions = makeActions __dirname,
  Object.assign {},
    getCaseHistoryFlow,
    repeatQuestionFlow,
    createServiceFlow,
    answerFlow

module.exports = actions
