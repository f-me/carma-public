define ["text!tpl/screens/kpi/group.html"
        "json!/cfg/model/GroupKPI?view=kpi"
        "model/main"
        "model/fields"
        "sync/datamap"
        "utils"
        "lib/current-user"
  ], (Tpl, Model, Main, Fs, Map, U, Usr) ->

  key = "kpi-group"

  mp = new Map.Mapper(Model)

  template: Tpl
  constructor: (view, opts) ->
    $("#group-screen").addClass("active")
    s = (Usr.readStuff key) || {}
    int = s?.interval or
      [ (new Date).toString("dd.MM.yyyy 00:00:00")
      , (new Date).toString("dd.MM.yyyy HH:mm:ss")
      ]
    interval = Fs.interval ko.observable(int)

    kvm = Main.buildKVM Model
    flt = ko.observable ""
    sorted = ko.sorted
      kvms: Model.fields
      filters:
        f: (fld) ->
          return true if _.isEmpty flt()
          U.checkMatch flt(), fld.meta.label

    sorted.change_filters "f"

    spinner = ko.observable(false)
    fetchData = ->
      return unless interval.begin() or interval.end()
      sint = _.map interval(), (v) -> Map.c2s(v, 'UTCTime')
      spinner(true)
      $.getJSON "/kpi/group/#{sint[0]}/#{sint[1]}", (d) ->
        for k, v of mp.s2cObj d
          kvm[k](v)
        spinner(false)

    ctx = {kvm, filter: flt, fields: sorted, fetchData, interval, spinner}
    ko.applyBindings ctx, $("#settings")[0]
    fetchData()

  destructor: ->
