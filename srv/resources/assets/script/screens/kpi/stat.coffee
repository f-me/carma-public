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

    flds = ko.observable _.map Model.fields, (f) ->
      {name: f.name, label: f.meta.label, show: ko.observable(false)}

    filter = ko.observable("")

    filted = ko.computed ->
      fs = ko.utils.unwrapObservable flds
      return fs if _.isEmpty filter()
      _.filter fs, (f) ->
        f.label.toLowerCase().indexOf(filter().toLowerCase()) >= 0

    ctx = {fields: filted, kvms: [], filter: filter}
    ko.applyBindings(ctx, $("#kpi-list-inner")[0])
    ko.applyBindings(ctx, $("#tbl")[0])

    $("##{opts.model}-screen").addClass("active")
