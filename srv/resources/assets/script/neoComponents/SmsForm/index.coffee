{ko} = require "carma/vendor"
{data} = require "carma/data"
{store} = require "carma/neoComponents/store"
{closeSmsForm} = require "carma/neoComponents/store/smsForm/actions"
require "./styles.less"

smsTemplates = data.model.SmsTemplate.filter (x) => x.isActive


class SmsFormViewModel
  constructor: () ->
    @smsTemplates = ko.observableArray smsTemplates

    @appState = ko.observable store.getState()
    @unsubscribeFromAppState = store.subscribe => @appState store.getState()

    @isShown = ko.pureComputed => @appState().smsForm.isShown
    @phone   = ko.pureComputed => @appState().smsForm.phone
    @caseId  = ko.pureComputed => @appState().smsForm.caseId
    @message = ko.observable "…"

    @fadeIn = ko.pureComputed @isShown
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 1}

    fadeOut = ko.pureComputed @fadeIn
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 500}

    @isVisible = ko.computed => @isShown() || fadeOut()

  dispose: =>
    do @unsubscribeFromAppState

  closeForm: () =>
    store.dispatch closeSmsForm()

  handleOverlayClick: (model, {target}) =>
    do @closeForm if target.classList.contains "is-overlay"


module.exports =
  componentName: "sms-form"

  component:
    template:  require "./template.pug"
    viewModel: SmsFormViewModel
