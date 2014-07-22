define ["text!tpl/screens/kpi/stat.html"], (tpl) ->
  template: tpl
  constructor: (_, opts) ->
    $("#kpi-list").on "click", ->
      if $("#kpi-list-inner").hasClass("in")
        $("#kpi-list-inner")
          .removeClass("in")
          .slideUp()
      else
        $("#kpi-list-inner")
          .addClass("in")
          .slideDown()


    $("##{opts.model}-screen").addClass("active")
