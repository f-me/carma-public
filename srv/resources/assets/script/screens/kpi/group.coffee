{$, _, ko} = require "carma/vendor"
{tpl} = require "carma/lib/template"

Model = require("carma/data").data.cfg.m.v.kpi.GroupKPI

Main = require "carma/model/main"
Fs   = require "carma/model/fields"
Map  = require "carma/sync/datamap"
Usr  = require "carma/lib/current-user"
U    = require "carma/utils"

Tpl = tpl require "carma-tpl/screens/kpi/group.pug"

key = "kpi-group"
mp  = new Map.Mapper(Model)

module.exports =
  template: Tpl
  constructor: (view, opts) ->
    $("#group-screen").addClass("active")
    s = (Usr.readStuff key) || {}
    int = s?.interval or
      [ (new Date).toString("dd.MM.yyyy 00:00:00")
      , (new Date).toString("dd.MM.yyyy HH:mm:ss")
      ]
    interval = Fs.interval ko.observable(int)
    interval.correct = ko.computed ->
      sint = _.map interval(), (v) -> Map.c2s(v, 'UTCTime')
      sint[0] < sint[1]

    interval.subscribe (v) ->
      return unless interval.begin or interval.end
      Usr.writeStuff key, { interval: interval() }

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
      return unless interval.correct()

      spinner(true)
      # Clear old values as new results may not contain all fields
      for k, v of Model.fields
        kvm[v.name] null
      $.getJSON "/kpi/group/#{sint[0]}/#{sint[1]}", (d) ->
        for k, v of mp.s2cObj d
          kvm[k](v)
        spinner(false)

    ctx = {kvm, filter: flt, fields: sorted, fetchData, interval, spinner}
    ko.applyBindings ctx, $("#settings")[0]
    fetchData()

  destructor: ->
