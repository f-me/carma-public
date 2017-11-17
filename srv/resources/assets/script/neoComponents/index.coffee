{ko} = require "carma/vendor"

{TopLevelModalsViewModel} =
  require "carma/neoComponents/TopLevelModalsViewModel"

module.exports =
  init: () ->
    components = [
      require "carma/neoComponents/SmsForm"
    ]

    for x in components
      ko.components.register x.componentName, x.component

    ko.applyBindings (new TopLevelModalsViewModel),
      document.getElementById "top-level-modals"
