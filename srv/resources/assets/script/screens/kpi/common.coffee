define ["lib/current-user"
        "model/utils"
        "utils"
        "base64"
  ], (Usr, MU, U, B64) ->

  initCtx: (key, model, customInit) ->
    $("#settings-label").on "click", ->
      if $("#kpi-list-inner").hasClass("in")
        $("#kpi-list-inner").removeClass("in").slideUp()
      else
        $("#kpi-list-inner").addClass("in").slideDown()

    settings = (Usr.readStuff key) || {}

    interesting = (f) ->
      not (f.meta.invisible or _.contains ["userid", "day"], f.name)

    # fields without userid and day, they will be always rendered
    # by custom code
    flds = _.map _.filter(model.fields, (f) -> interesting(f)), (f) ->
      show = settings?.fields?[f.name] || false
      {name: f.name, label: f.meta.label, show: ko.observable(show)}

    filter = ko.observable("")

    filted = ko.computed ->
      fs = ko.utils.unwrapObservable flds
      return fs if _.isEmpty filter()
      _.filter fs, (f) ->
        f.label.toLowerCase().indexOf(filter().toLowerCase()) >= 0

    kvms = ko.observableArray([])
    flt = ko.observable("")
    sorted = ko.sorted
      kvms: kvms
      sorters: MU.buildSorters model
      filters:
        kvmFilter: (kvm) ->
          return true if _.isEmpty flt()
          U.kvmCheckMatch(flt(), kvm,
            if model.name == 'OperKPI'
              {allowed: ['userid', 'currentState']})

    sorted.change_filters "kvmFilter"

    csv = ko.computed
      read: ->
        r = ""
        for f in flds when f.show()
          r += "#{f.label};"
        r += "\n"
        for s in sorted()
          for f in flds when f.show()
            r += "#{s[f.name].text()};"
          r += "\n"
        "data:application/octet-stream; base64, #{B64.encode('\uFEFF' + r)}"

      deferEvaluation: true

    tblCtx = {fields: flds, kvms: sorted }
    settingsCtx =
      fields: filted
      settingsFilter: filter
      kvmsFilter: flt
      csv: csv


    {tblCtx, settingsCtx, dumpSettings} =
      customInit(settings, settingsCtx, tblCtx, dumpSettings, kvms)

    dumper = ->
      s = {}
      s.fields = {}
      _.map flds, (v) -> s.fields[v.name] = v.show()
      if _.isObject dumpSettings
        for k, v of dumpSettings
          s[k] = ko.utils.unwrapObservable v
      Usr.writeStuff key, s

    if _.isObject dumpSettings
      for k, v of dumpSettings when ko.isObservable(v)
        do (v) -> v.subscribe dumper

    _.map flds, (v) -> v.show.subscribe dumper

    return {tblCtx, settingsCtx}
