define ["text!tpl/screens/kpi/oper.html"
        "json!/cfg/model/OperKPI?view=kpi"
        "model/main"
        "sync/dataMap"
        "lib/messenger"
        "screens/kpi/common"
  ], (Tpl, Model, Main, Map, WS, Common) ->

  stuffKey = "kpi-oper"

  mp = new Map.Mapper(Model)
  update = (kvmsh) -> (data) ->
    for d in data
      do (d) ->
        a = mp.s2cObj d
        kvm = kvmsh[a.userid]
        for k, v of a
          do (k, v) -> kvm[k](v)

  template: Tpl
  constructor: (view, opts) ->
    $("#oper-screen").addClass("active")
    {tblCtx, settingsCtx} = Common.initCtx "kpi-stat", Model,
      (s, sCtx, tCtx, d, kvms) ->
        sCtx.overdue = ko.observable(s.overdue)
        tCtx.overdue = sCtx.overdue
        sCtx.overdue.text = ko.computed
          read:      -> Math.floor(sCtx.overdue() / 60)
          write: (v) -> 60 * parseInt(v)

        sCtx.hideOffline = ko.observable(s.hideOffline)
        tCtx.hideOffline = sCtx.hideOffline

        $.getJSON "/kpi/oper", (d) ->
          kvms _.map d, (m) -> Main.buildKVM Model, { fetched: mp.s2cObj m }
          $('body').spin false
          kvmsh = arrToObj ((k) -> k.userid()), kvms()
          WS.subscribe "oper-kpi", update(kvmsh)

        settingsCtx: sCtx
        tblCtx:      tCtx
        dumpSettings: { overdue: sCtx.overdue, hideOffline: sCtx.hideOffline }

    ko.applyBindings(settingsCtx, $("#settings")[0])
    ko.applyBindings(tblCtx, $("#tbl")[0])

  # FIXME: find better way to cleanup (why the hell we have to do this by hand?)
  destructor: ->
    ko.dataFor($("#tbl")[0]).kvms.clean()
