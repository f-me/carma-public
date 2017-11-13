{ko} = require "carma/vendor"
{tpl} = require "carma/lib/template"
{data} = require "carma/data"
require "./styles.less"

smsTemplates = data.model.SmsTemplate.filter (x) => x.isActive


class SmsFormViewModel
  constructor: ({valuesModel: {@phone, @caseId}, @closeForm, isActive}) ->
    @smsTemplates = ko.observableArray smsTemplates
    @message = ko.observable "…"

    @fadeIn = ko.pureComputed isActive
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 1}

    fadeOut = ko.pureComputed @fadeIn
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 500}

    @isVisible = ko.computed => isActive() || fadeOut()

  dispose: =>

  handleOverlayClick: (model, {target}) =>
    do @closeForm if target.classList.contains "is-overlay"


class SmsFormValuesModel
  constructor: ->
    @phone       = ko.observable ""
    @caseId      = ko.observable ""
    @caseCity    = ko.observable ""
    @caseAddress = ko.observable ""
  dispose: =>

  fill: ({phone, caseId, caseCity, caseAddress}) =>
    @phone       phone
    @caseId      caseId
    @caseCity    caseCity
    @caseAddress caseAddress

  reset: =>
    @phone       ""
    @caseId      ""
    @caseCity    ""
    @caseAddress ""


module.exports = {
  name:      "sms-form"
  template:  tpl require "./template.pug"
  viewModel: SmsFormViewModel

  SmsFormValuesModel
}
