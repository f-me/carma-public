define ["text!tpl/screens/kpi/stat.html"
        "json!/cfg/model/FrontKPI?view=kpi"
  ], (Tpl, Model) ->

  template: Tpl
  constructor: (view, opts) ->
    $("#settings-label").on "click", ->
      if $("#kpi-list-inner").hasClass("in")
        $("#kpi-list-inner").removeClass("in").slideUp()
      else
        $("#kpi-list-inner").addClass("in").slideDown()

    flds = _.map Model.fields, (f) ->
      {name: f.name, label: f.meta.label, show: ko.observable(false)}

    window.kkk = flds
    ko.applyBindings(flds, $("#kpi-list-inner")[0])
    ko.applyBindings({fields: flds, kvms: []}, $("#tbl")[0])

    $("##{opts.model}-screen").addClass("active")
