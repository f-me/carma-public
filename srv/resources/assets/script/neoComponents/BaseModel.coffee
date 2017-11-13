{ko} = require "carma/vendor"
{SmsFormValuesModel} = require "carma/neoComponents/SmsForm"


class BaseModel
  constructor: ->
    @smsFormValuesModel = new SmsFormValuesModel
    @smsFormActive = ko.observable false
  dispose: =>
    do @smsFormValuesModel.dispose

  showSmsForm: ({phone, caseId, caseCity, caseAddress}) =>
    @smsFormValuesModel.fill {phone, caseId, caseCity, caseAddress}
    @smsFormActive true

  closeSmsForm: =>
    @smsFormActive false
    do @smsFormValuesModel.reset


module.exports =
  baseModel: new BaseModel
