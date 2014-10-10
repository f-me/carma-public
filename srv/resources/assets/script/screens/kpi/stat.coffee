define ["text!tpl/screens/kpi/stat.html"
        "json!/cfg/model/StatKPI?view=kpi"
        "model/main"
        "model/fields"
        "sync/datamap"
        "screens/kpi/common"
  ], (Tpl, Model, Main, Fs, DMap, Common) ->

  stuffKey = "kpi-stat"

  template: Tpl
  constructor: (view, opts) ->
    $("#stat-screen").addClass("active")

    spinner = ko.observable(false)
    {tblCtx, settingsCtx} = Common.initCtx "kpi-stat", Model,
      (s, sCtx, tCtx, d, kvms) ->
        int = s?.interval or
         [ (new Date).toString("dd.MM.yyyy 00:00:00")
         , (new Date).toString("dd.MM.yyyy HH:mm:ss")
         ]
        sCtx.interval = Fs.interval ko.observable(int)

        updateTbl = (int) ->
          return if _.isNull int
          sint = _.map int, (v) -> DMap.c2s(v, 'UTCTime')
          spinner true
          $.getJSON "/kpi/stat/#{sint[0]}/#{sint[1]}", (d) ->
            kvms _.map d, (m) -> Main.buildKVM Model, { fetched: m }
            spinner false

        updateTbl sCtx.interval()
        sCtx.fetchData = -> updateTbl(sCtx.interval())

        settingsCtx: sCtx
        tblCtx:      tCtx
        dumpSettings: { interval: sCtx.interval }

    ko.applyBindings({settingsCtx, tblCtx, spinner}, $("#stat-kpi-content")[0])
    # ko.applyBindings(settingsCtx, $("#settings")[0])
    # ko.applyBindings(tblCtx, $("#tbl")[0])

  destructor: ->
    ko.dataFor($("#tbl")[0]).kvms.clean()
