{ko, Immutable: {Map, List}} = require "carma/vendor"
{backgrounds} = require "./precompiled"
{store} = require "carma/neoComponents/store"

{
  repeatQuestionRequest
  createServiceRequest
  answerRequest
} =
  require "carma/neoComponents/store/diagTree/show/actions"

{
  CaseHistoryList
  CaseHistoryItem
} = require "carma/neoComponents/store/diagTree/show/models"

require "./styles.less"

yesNoRegs = [/^да/i, /^нет/i]


# This component isn't supposed to be mounted at top-level,
# it"s supposed to always be wrapped by diag-tree-show.
class DiagTreeShowInsideViewModel
  # caseModel: see store/diagTree/show/models
  constructor: ({@caseModel}) ->
    @subscriptions = [] # Mutable

    @originalHistory = ko.pureComputed => @caseModel().get "history"
    @isProcessing = ko.pureComputed => @caseModel().get "isLoading"

    @history = ko.pureComputed =>
      @originalHistory().onlyNotDeprecated().toArray()

    @showDeprecated = ko.observable null
    @hoverId = ko.observable null

    @slideId = ko.observable if @originalHistory().size > 0 \
                                then @originalHistory().last().get "id"
                                else null

    defaultSlide = new CaseHistoryItem
    @slide = ko.pureComputed =>
      slideId = @slideId() # setting dependency
      found = @originalHistory().find (x) -> x.get("id") is slideId
      if found? then found else defaultSlide

    @subscriptions.push @originalHistory.subscribeWithOld (newVal, oldVal) =>
      if oldVal.size is 0 and newVal.size > 0
        @slideId newVal.last().get "id"

    @slideAction     = ko.pureComputed => @slide().getIn ["actions", 0], null
    @slideResources  = ko.pureComputed => @slide().get("resources").toArray()
    @slideAnswers    = ko.pureComputed => @slide().get("answers").toArray()
    @slideAnswerIdx  = ko.pureComputed => @slide().get("answerIx")
    @isSlideAnswered = ko.pureComputed => @slideAnswerIdx() isnt null
    @slideBody       = ko.pureComputed => @slide().get("body").trim()

    @isItYesNoAnswer = ko.pureComputed =>
      slide = @slide()
      slide.get("answers").size is 2 and (
        tester = (x, i) => x.test slide.getIn ["answers", i, "header"]
        yesNoRegs.every(tester) or yesNoRegs.reverse().every(tester)
      )

  dispose: =>
    do x.dispose for x in @subscriptions

  prevHistory: (id) => @originalHistory().getPreviousById id
  hasPrevHistory: (id) => @prevHistory(id).size > 0
  answerBg: (idx) => backgrounds[idx % backgrounds.length]

  # handlers

  showDeprecatedAnswers: (model) =>
    return if @isProcessing()
    @showDeprecated model.get "id"

  hideDeprecatedAnswers: =>
    return if @isProcessing()
    @showDeprecated null

  selectHistory: (model) =>
    return if @isProcessing()
    @slideId model.get "id"

  onHistoryMouseEnter: (model) =>
    return if @isProcessing()
    @hoverId model.get "id"

  onHistoryMouseLeave: (model) =>
    return if @isProcessing()
    @hoverId null if @hoverId() is model.get "id"

  onHistoryFocus: (args...) => @onHistoryMouseEnter args...
  onHistoryBlur: (args...) => @onHistoryMouseLeave args...

  repeatQuestion: (model, e) =>
    do e.target.blur
    return if @isProcessing()

    store.dispatch repeatQuestionRequest new repeatQuestionRequest.Payload
      caseId: @caseModel().get "id"
      historyItemId: model.get "id"

  handleAnswer: (answerIdx, model) =>
    return if @isProcessing()

    store.dispatch answerRequest new answerRequest.Payload
      caseId      : @caseModel().get "id"
      slideId     : @slide().get "id"
      nextSlideId : model.get "nextSlide"
      answerIndex : answerIdx

  handleSlideAction: =>
    return if @isProcessing()
    caseId = @caseModel().get "id"

    payload = new createServiceRequest.Payload {
      serviceModelName: @slideAction().get "svc"
      caseId
    }

    store.dispatch createServiceRequest payload
      .then ->
        window.localStorage.setItem "DiagTree/#{caseId}/newSvc", true
        window.alert "Готово. Опрос будет закрыт."
        do window.close


module.exports =
  componentName: "diag-tree-show-inside"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeShowInsideViewModel
