define ["text!tpl/screens/kpi/stat.html"], (tpl) ->
  template: tpl
  constructor: (_, opts) ->
    $("##{opts.model}-screen").addClass("active")
