{ko} = require "carma/vendor"

{TopLevelModalsViewModel} =
  require "carma/neoComponents/TopLevelModalsViewModel"

module.exports =
  registerComponents: ->
    components = [
      require "carma/neoComponents/SmsForm"
      require "carma/neoComponents/AvarcomTasks"
    ]

    for x in components
      ko.components.register x.componentName, x.component

  initTopLevelModals: ->
    ko.applyBindings (new TopLevelModalsViewModel),
      document.getElementById "top-level-modals"
