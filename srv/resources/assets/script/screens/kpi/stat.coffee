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
          $('body').spin 'huge', '#777'
          $.getJSON "/kpi/stat/#{sint[0]}/#{sint[1]}", (d) ->
            kvms _.map d, (m) -> Main.buildKVM Model, { fetched: m }
            $('body').spin false

        sCtx.interval.subscribe updateTbl
        updateTbl sCtx.interval()

        settingsCtx: sCtx
        tblCtx:      tCtx
        dumpSettings: { interval: sCtx.interval }

    ko.applyBindings(settingsCtx, $("#settings")[0])
    ko.applyBindings(tblCtx, $("#tbl")[0])

  destructor: ->
    ko.dataFor($("#tbl")[0]).kvms.clean()
