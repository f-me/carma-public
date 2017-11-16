{ko} = require "carma/vendor"
{BaseViewModel} = require "carma/neoComponents/BaseViewModel"

module.exports =
  init: () ->
    components = [
      require "carma/neoComponents/SmsForm"
    ]

    for x in components
      ko.components.register x.componentName, x.component

    ko.applyBindings (new BaseViewModel),
      document.getElementById "top-level-modals"
