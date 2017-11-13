# Global processing configuration
#
# Uses "ProcessingConfig" model

{tpl} = require "carma/lib/template"
main = require "carma/model/main"
template = tpl require "carma-tpl/screens/processingConfig.pug"

module.exports = {
  template
  constructor: () ->
    view = "config-form"
    main.modelSetup("ProcessingConfig") \
      view,
      {id: window.global.idents("ProcessingConfig").main},
      {permEl: "config-permissions"}
}
