# Global processing configuration
#
# Uses "ProcessingConfig" model

main = require "carma/model/main"
template = require "carma-tpl/screens/processingConfig.pug"

module.exports = {
  template
  constructor: () ->
    view = "config-form"
    main.modelSetup("ProcessingConfig") \
      view,
      {id: window.global.idents("ProcessingConfig").main},
      {slotsee: ["config-permissions"]}
}
