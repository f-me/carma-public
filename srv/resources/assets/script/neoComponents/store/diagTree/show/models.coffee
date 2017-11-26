{_: {pick, memoize}, Immutable: {Record, List}} = require "carma/vendor"


class CaseHistoryItemAnswersItem extends Record(
  # scalar types
  nextSlide : 0
  header    : ""
  text      : ""

  # unknown types
  # action    : {}
)
  @plainObjScalarProps = ["nextSlide", "header", "text"]
  @fromPlain = (plainObj) => new @ pick plainObj, @plainObjScalarProps


class CaseHistoryItemAnswersList extends List
  @Item: CaseHistoryItemAnswersItem
  @fromPlain: (plainArr) => List plainArr.map (x) => @Item.fromPlain x

  constructor: (args...) ->
    list = super args...
    return list


class CaseHistoryItem extends Record(
  # scalar types
  id           : 0
  header       : ""
  body         : "" # html code
  answerIx     : null # number
  answeredBy   : null # string
  answerTime   : null # string
  deprecatedBy : null # number

  # complex types
  answers      : List()
  # actions      : List()
  # resources    : List()
)
  @AnswersItem = CaseHistoryItemAnswersItem

  @plainObjScalarProps =
    ["id", "header", "body", "answerIx", "answeredBy", "answerTime"]

  @fromPlain: (plainObj) =>
    obj = pick plainObj, @plainObjScalarProps
    new @ Object.assign obj,
      answers: List plainObj.answers.map (x) => @AnswersItem.fromPlain x


class CaseHistoryList extends List
  @Item: CaseHistoryItem
  @fromPlain: (plainArr) => new @ plainArr.map (x) => @Item.fromPlain x

  constructor: (args...) ->
    list = super args...
    list.onlyNotDeprecated = memoize @onlyNotDeprecated
    list.getPreviousById = memoize @getPreviousById
    return list

  onlyNotDeprecated: -> @filter (x) -> x.get("deprecatedBy") is null
  getPreviousById: (id) -> @filter (x) -> x.get("deprecatedBy") is id


module.exports = {
  CaseHistoryList
  CaseHistoryItem
}
