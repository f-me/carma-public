# Global processing configuration
#
# Uses "ProcessingConfig" model
define [ "screens/processingConfig.jade"
       , "model/main"], (tpl, main) ->
  template: tpl()
  constructor: () ->
    view = "config-form"
    main.modelSetup("ProcessingConfig") view, {id: global.idents("ProcessingConfig").main},
      {permEl: "config-permissions"}
