{ko: {observable}} = require "carma/vendor"


class SmsFormValuesModel
  constructor: ->
    @phone       = observable ""
    @caseId      = observable ""
    @caseCity    = observable ""
    @caseAddress = observable ""
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


class BaseModel
  constructor: ->
    @smsFormValuesModel = new SmsFormValuesModel
    @smsFormActive = observable false
  dispose: =>
    do @smsFormValuesModel.dispose

  showSmsForm: ({phone, caseId, caseCity, caseAddress}) =>
    @smsFormValuesModel.fill {phone, caseId, caseCity, caseAddress}
    @smsFormActive true

  closeSmsForm: =>
    @smsFormActive false
    do @smsFormValuesModel.reset


module.exports.baseModel = new BaseModel
