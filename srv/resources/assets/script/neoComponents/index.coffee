{ko} = require "carma/vendor"
{baseModel} = require "carma/neoComponents/BaseModel"

module.exports =
  init: () ->
    ko.components.register "SmsForm", require "carma/neoComponents/SmsForm"
    ko.applyBindings baseModel, document.getElementById "sms-send-form"
