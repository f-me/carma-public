{ko} = require "carma/vendor"

registerComponents = ->
  components = [
    require "carma/neoComponents/SmsForm"
    require "carma/neoComponents/AvarcomTasks"
    require "carma/neoComponents/DiagTree/Show"
    require "carma/neoComponents/DiagTree/Show/Inside"
    require "carma/neoComponents/DiagTree/Editor"
    require "carma/neoComponents/PermissionBlock"
    require "carma/neoComponents/Navbar"
    require "carma/neoComponents/UploadFileField"
  ]

  for x in components
    ko.components.register x.componentName, x.component

module.exports = {registerComponents}
