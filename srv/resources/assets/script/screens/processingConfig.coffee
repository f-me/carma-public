# Global processing configuration
#
# Uses "ProcessingConfig" model
define [ "text!tpl/screens/processingConfig.html"
       , "model/main"
       ], (tpl, main) ->
  template: tpl
  constructor: () ->
    view = "config-form"
    main.modelSetup("ProcessingConfig") \
      view,
      {id: global.idents("ProcessingConfig").main},
      {permEl: "config-permissions"}
