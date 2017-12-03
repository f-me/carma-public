{ko} = require "carma/vendor"
{simpleFuzzySearch} = require "carma/lib/search"
{data} = require "carma/data"
{store} = require "carma/neoComponents/store"

{
  closeSmsForm
  sendSmsFormRequest
} = require "carma/neoComponents/store/smsForm/actions"

require "./styles.less"

smsTemplates = data.model.SmsTemplate.filter (x) => x.isActive
storeSelector = -> store.getState().get "smsForm"


class SmsFormViewModel
  constructor: ->
    # Connector to store
    @appState = ko.observable storeSelector()
    @unsubscribeFromAppState = store.subscribe => @appState storeSelector()

    @subscriptions = [] # Mutable

    # Pure value from store
    @isProcessing = ko.pureComputed => @appState().get "isProcessing"
    @isFailed     = ko.pureComputed => @appState().get "isFailed"
    isShown       = ko.pureComputed => @appState().get "isShown"

    @phone = ko.observable @appState().get "phone"
      .extend validate: (x) -> true unless /^\+\d{11}$/.test x

    @caseId      = ko.observable @appState().get "caseId"
    @caseCity    = ko.observable @appState().get "caseCity"
    @caseAddress = ko.observable @appState().get "caseAddress"

    # Overwrite form's values with ones from store when form is just shown
    @subscriptions.push isShown.subscribe (value) =>
      return unless value # if form is just closed we don't have to do anything

      @phone       @appState().get "phone"
      @caseId      @appState().get "caseId"
      @caseCity    @appState().get "caseCity"
      @caseAddress @appState().get "caseAddress"

      @smsTemplate null
      @message     ""

    @message = ko.observable("").extend validate: (x) -> true if x.trim() is ""

    # Delaying just for a moment to start animation after element is shown
    @fadeIn = ko.pureComputed isShown
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 100}

    # Some gap for fade-out animation
    fadeOut = ko.pureComputed @fadeIn
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 500}

    # Property that indicates if root elements must be shown,
    # not really visible for user, but visible for browser (display: block).
    @isVisible = ko.computed => isShown() or fadeOut()

    @smsTemplate = ko.observable null

    @subscriptions.push @smsTemplate.subscribe (x) =>
      return unless x?
      {text} = do -> return tpl for tpl in smsTemplates when tpl.label is x

      x = text
        .replace /\$phone\$/g,                     @phone()
        .replace /\$case\.id\$/g,                  @caseId()
        .replace /\$case\.city\$/g,                @caseCity()
        .replace /\$case\.caseAddress_address\$/g, @caseAddress()

      @message x

    @sendIsBlocked = ko.pureComputed => Boolean \
      @phone.validationError() or \
      @message.validationError() or \
      @isProcessing()

  dispose: =>
    do @unsubscribeFromAppState
    do x.dispose for x in @subscriptions

  closeForm: (model, e) ->
    do e?.preventDefault
    do e?.stopPropagation
    store.dispatch closeSmsForm()

  handleOverlayClick: (model, {target}) =>
    do @closeForm if target.classList.contains "is-overlay"

  smsTplFuzzySearchHandler: (q, cb) ->
    cb (x for {label: x} in smsTemplates when simpleFuzzySearch q, x)

  send: =>
    smsTpl = @smsTemplate()

    reqData = new sendSmsFormRequest.Payload
      phone:      @phone()
      caseId:     @caseId()
      message:    @message()
      templateId: _.find(smsTemplates, ({label: x}) => x is smsTpl)?.id ? null

    store.dispatch sendSmsFormRequest reqData
      .then =>
        @smsTemplate null
        @message     ""

  clearSmsTemplate: => @smsTemplate null


module.exports =
  componentName: "sms-form"

  component:
    template:  require "./template.pug"
    viewModel: SmsFormViewModel
