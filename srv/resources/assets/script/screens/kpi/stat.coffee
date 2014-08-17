define ["text!tpl/screens/kpi/stat.html"
        "json!/cfg/model/StatKPI?view=kpi"
        "model/main"
        "model/utils"
        "model/fields"
        "utils"
        "sync/datamap"
  ], (Tpl, Model, Main, MU, Fs, U, DMap) ->

  template: Tpl
  constructor: (view, opts) ->
    $("#settings-label").on "click", ->
      if $("#kpi-list-inner").hasClass("in")
        $("#kpi-list-inner").removeClass("in").slideUp()
      else
        $("#kpi-list-inner").addClass("in").slideDown()

    flds = ko.observable _.map Model.fields, (f) ->
      {name: f.name, label: f.meta.label, show: ko.observable(true)}

    filter = ko.observable("")

    filted = ko.computed ->
      fs = ko.utils.unwrapObservable flds
      return fs if _.isEmpty filter()
      _.filter fs, (f) ->
        f.label.toLowerCase().indexOf(filter().toLowerCase()) >= 0

    interval = Fs.interval ko.observable(
      [ (new Date).toString("dd.MM.yyyy 00:00:00")
      , (new Date).toString("dd.MM.yyyy HH:mm:ss")
      ])
    kvms = ko.observableArray([])
    flt = ko.observable ""
    sorted = ko.sorted
      kvms: kvms
      sorters: MU.buildSorters Model
      filters:
        kvmFilter: (kvm) ->
          return true if _.isEmpty flt()
          U.kvmCheckMatch(flt(), kvm)

    sorted.change_filters "kvmFilter"
    tblCtx = {fields: filted, kvms: sorted }
    settingsCtx =
      fields: filted
      settingsFilter: filter
      kvmsFilter: flt
      interval: interval
    ko.applyBindings(settingsCtx, $("#settings")[0])
    ko.applyBindings(tblCtx, $("#tbl")[0])

    $("#stat-screen").addClass("active")
    updateTbl = (int) ->
      return if _.isNull int
      sint = _.map int, (v) -> DMap.c2s(v, 'UTCTime')
      $('body').spin 'huge', '#777'
      $.getJSON "/kpi/stat/#{sint[0]}/#{sint[1]}", (d) ->
        kvms _.map d, (m) -> Main.buildKVM Model, { fetched: m }
        $('body').spin false

    interval.subscribe updateTbl
    updateTbl interval()


