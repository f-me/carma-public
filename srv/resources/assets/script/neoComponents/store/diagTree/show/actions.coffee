{_: {pick}, Immutable: {Record, List}} = require "carma/vendor"

{makeActions, catchFailure, fetchGet} =
  require "carma/neoComponents/store/utils"

CaseHistoryItemAnswersItem = Record
  # scalar types
  nextSlide : 0
  header    : ""
  text      : ""

  # unknown types
  # action    : {}

CaseHistoryItem = Record
  # scalar types
  id           : 0
  header       : ""
  body         : "" # html code
  answerIx     : null # number
  answeredBy   : null # string
  answerTime   : null # string

  # complex types
  answers      : List()
  # actions      : List()
  # resources    : List()

  # unknown types
  # deprecatedBy : null

# convert plain data to immutable structured object
getImmutableCaseHistoryItem = (history) -> # history: [Object]
  List history.map (historyItem) ->
    obj = pick historyItem,
      # scalar types
      ["id", "header", "body", "answerIx", "answeredBy", "answerTime"]

    new CaseHistoryItem Object.assign obj,
      answers: List historyItem.answers.map (answerItem) ->
        new CaseHistoryItemAnswersItem pick answerItem,
          ["nextSlide", "header", "text"]

actions = makeActions __dirname,
  getCaseHistoryRequest:
    Payload: Record
      caseId: 0

    handler: ({payload}, dispatch) ->
      caseId = payload.get "caseId"
      catchFailure dispatch, actions.getCaseHistoryFailure,
        fetchGet "/diag/history/#{caseId}"
          # constructing immutable data structure
          .then getImmutableCaseHistoryItem
          # dispatching successs action
          .then (history) ->
            success = actions.getCaseHistorySuccess
            dispatch success success.Payload {caseId, history}

  getCaseHistorySuccess:
    Payload: Record
      caseId: 0
      history: List()

  getCaseHistoryFailure:
    Payload: Record
      caseId: 0

module.exports = actions
