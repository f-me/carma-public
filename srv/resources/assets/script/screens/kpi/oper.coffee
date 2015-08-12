define ["text!tpl/screens/kpi/oper.html"
        "json!/cfg/model/OperKPI?view=kpi"
        "model/main"
        "sync/datamap"
        "lib/messenger"
        "screens/kpi/common"
        "utils"
  ], (Tpl, Model, Main, Map, WS, Common, Utils) ->

  stuffKey = "kpi-oper"

  mp = new Map.Mapper(Model)
  update = (kvmsh) -> (data) ->
    for d in data
      do (d) ->
        a = mp.s2cObj d
        kvm = kvmsh[a.userid]
        if kvm?
          for k, v of a
            do (k, v) -> kvm[k](v)

  tick = (kvms) -> ->
    for k in kvms()
      k.inCurrent(k.inCurrent() + 1)
      k[k.currentState()] k[k.currentState()]() + 1
      if k.currentState() != "LoggedOut"
        k.totalLoggedIn k.totalLoggedIn() + 1
      if _.contains ["Dinner", "Rest", "ServiceBreak", "NA"], k.currentState()
        k.totalRest k.totalRest() + 1
    return null

  ticker = null

  # Current call of a busy Front Office operator (always the first one
  # call of that operator)
  #
  # FIXME If an operator loses his CTI role, his last known AVAYA
  # state will still be preserved and shown on this screen
  mkCurrentCall = (k) -> ko.computed ->
    # Ignore non-CTI users or users not busy with a call action
    if k.currentAType() != global.idents("ActionType").call
      return null

    snapshot = k.lastAvayaSnapshot()
    sortedCalls = _.sortBy(
      _.keys(snapshot.calls),
      (k) -> new Date(snapshot.calls[k].start)
      )
    return sortedCalls[0]

  mkListenTo = (k) -> () ->
    if k._meta.currentCall()? && global.CTIPanel?
      global.CTIPanel.bargeIn k._meta.currentCall(), "Silent"

  mkJoinTo = (k) -> () ->
    if k._meta.currentCall()? && global.CTIPanel?
      global.CTIPanel.bargeIn k._meta.currentCall(), "Active"

  mkLogoutFromBusy = (k) -> ko.computed ->
    k.lastState() == 'Busy' &&
    k.currentState() == 'LoggedOut' &&
    k.inCurrent() <= 15 * 60

  aDict = Utils.newModelDict "ActionType", false, dictionaryLabel: 'maxSeconds'

  mkOverDue = (k, overdue) -> ko.computed ->
    secs = parseInt aDict.getLab k.currentAType()
    k.currentState() == 'Busy' && (
      (k.inCurrent() > overdue()) || (k.inCurrent() > secs))

  mkVisible = (k, hideOffline,outFromBusy) -> ko.computed ->
    if hideOffline()
      outFromBusy() or k.currentState() != "LoggedOut"
    else
      true

  template: Tpl
  constructor: (view, opts) ->
    uDict = Utils.newModelDict "Usermeta", false, dictionaryLabel: 'grp'

    $("#oper-screen").addClass("active")
    {tblCtx, settingsCtx} = Common.initCtx "kpi-stat", Model,
      (s, sCtx, tCtx, d, kvms) ->
        sCtx.overdue = ko.observable(s.overdue)
        tCtx.overdue = sCtx.overdue
        sCtx.overdue.text = ko.computed
          read:      -> Math.floor(sCtx.overdue() / 60)
          write: (v) -> sCtx.overdue 60 * parseInt(v)

        sCtx.hideOffline = ko.observable(s.hideOffline)
        tCtx.hideOffline = sCtx.hideOffline

        $.getJSON "/kpi/oper", (d) ->
          kvms _.map d, (m) ->
            k = Main.buildKVM Model, { fetched: mp.s2cObj m }
            k.grp = uDict.getLab k.userid()
            if !_.isEmpty(k.grp)
              k.useridGrp = "#{k.useridLocal()} (#{k.grp})"
            else
              k.useridGrp = k.useridLocal()
            k._meta.currentCall    = mkCurrentCall(k)
            k._meta.listenTo       = mkListenTo(k)
            k._meta.joinTo         = mkJoinTo(k)
            k._meta.logoutFromBusy = mkLogoutFromBusy(k)
            k._meta.overdue        = mkOverDue(k, sCtx.overdue)
            k._meta.visible        =
              mkVisible(k, sCtx.hideOffline, k._meta.logoutFromBusy)
            k
          $('body').spin false
          kvmsh = arrToObj ((k) -> k.userid()), kvms()
          WS.subscribe "oper-kpi", update(kvmsh)
          ticker = setInterval tick(kvms), 1000

        settingsCtx: sCtx
        tblCtx:      tCtx
        dumpSettings: { overdue: sCtx.overdue, hideOffline: sCtx.hideOffline }

    ko.applyBindings(settingsCtx, $("#settings")[0])
    ko.applyBindings(tblCtx, $("#tbl")[0])

  # FIXME: find better way to cleanup (why the hell we have to do this by hand?)
  destructor: ->
    clearInterval(ticker)
    ko.dataFor($("#tbl")[0]).kvms.clean()
