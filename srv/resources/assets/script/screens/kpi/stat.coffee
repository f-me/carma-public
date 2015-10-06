define ["screens/kpi/stat.jade"
        "json!/cfg/model/StatKPI?view=kpi"
        "model/main"
        "model/fields"
        "sync/datamap"
        "screens/kpi/common"
        "utils"
  ], (Tpl, Model, Main, Fs, Map, Common, Utils) ->

  stuffKey = "kpi-stat"
  mp = new Map.Mapper(Model)
  template: Tpl()
  constructor: (view, opts) ->
    $("#stat-screen").addClass("active")

    uDict = Utils.newModelDict "Usermeta", false, dictionaryLabel: 'grp'

    spinner = ko.observable(false)
    {tblCtx, settingsCtx} = Common.initCtx "kpi-stat", Model,
      (s, sCtx, tCtx, d, kvms) ->
        int = s?.interval or
         [ (new Date).toString("dd.MM.yyyy 00:00:00")
         , (new Date).toString("dd.MM.yyyy HH:mm:ss")
         ]
        sCtx.interval = Fs.interval ko.observable(int)
        sCtx.interval.correct = ko.computed ->
          sint = _.map sCtx.interval(), (v) -> Map.c2s(v, 'UTCTime')
          sint[0] < sint[1]

        sCtx.cases_amount = ko.observable()
        sCtx.files_attached = ko.observable()

        updateTbl = (int) ->
          return if _.isNull int
          sint = _.map int, (v) -> Map.c2s(v, 'UTCTime')
          return unless sCtx.interval.correct()
          spinner true
          $.getJSON "/kpi/stat/#{sint[0]}/#{sint[1]}", (data) ->
            ks = for m in data
              do (m) ->
                kvm = Main.buildKVM Model, {fetched: mp.s2cObj m}
                kvm.grp = uDict.getLab kvm.userid()
                if !_.isEmpty(kvm.grp)
                  kvm.useridGrp = "#{kvm.useridLocal()} (#{kvm.grp})"
                else
                  kvm.useridGrp = kvm.useridLocal()
                kvm.showDetails = ko.observable(false)
                kvm.showDetails.toggle = do (kvm) -> ->
                  kvm.showDetails !kvm.showDetails()
                kvm.showDetails.loading = ko.observable(false)
                # daysArr is inner observable, that will actually contain
                # perday kpi, kvm.days will be evaluated on demand and will
                # fill daysArr after fetching data
                daysArr = ko.observable(false)
                kvm.days = ko.computed
                  deferEvaluation: true
                  read: ->
                    return daysArr() if daysArr()
                    kvm.showDetails.loading true
                    $.getJSON "/kpi/stat/#{kvm.userid()}/#{sint[0]}/#{sint[1]}",
                      (ds) ->
                        daysArr _.map ds, (d) ->
                          Main.buildKVM Model, {fetched: mp.s2cObj d}
                        kvm.showDetails.loading false
                    return []

                kvm

            kvms ks
            spinner false
          $.getJSON "kpi/statFiles/#{sint[0]}/#{sint[1]}", ([c, f]) ->
            sCtx.cases_amount c
            sCtx.files_attached f

        updateTbl sCtx.interval()
        sCtx.fetchData = -> updateTbl(sCtx.interval())

        settingsCtx: sCtx
        tblCtx:      tCtx
        dumpSettings: { interval: sCtx.interval }

    ko.applyBindings({settingsCtx, tblCtx, spinner, kvms: tblCtx.kvms},
                     $("#stat-kpi-content")[0])
    # ko.applyBindings(settingsCtx, $("#settings")[0])
    # ko.applyBindings(tblCtx, $("#tbl")[0])

  destructor: ->
    ko.dataFor($("#tbl")[0]).kvms.clean()
