{ko} = require "carma/vendor"
{baseModel} = require "carma/neoComponents/BaseModel"

module.exports =
  init: () ->
    do ->
      {name, template, viewModel} = require "carma/neoComponents/SmsForm"
      ko.components.register name, {template, viewModel}
      ko.applyBindings baseModel, document.getElementById "top-level-modals"
