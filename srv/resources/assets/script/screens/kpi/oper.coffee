define ["screens/kpi/oper.jade"
        "model/main"
        "sync/datamap"
        "lib/messenger"
        "screens/kpi/common"
        "utils"
  ], (Tpl, Main, Map, WS, Common, Utils) ->

  stuffKey = "kpi-oper"

  fetchJson = (done) ->
    fetch('/cfg/model/OperKPI?view=kpi', {credentials: 'same-origin'})
      .then((resp) -> resp.json())
      .then done


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

  # Currently active call of a busy Front Office operator (always the
  # first one answered non-held call of that operator)
  #
  # FIXME If an operator loses his CTI role, his last known AVAYA
  # state will still be preserved and shown on this screen
  mkCurrentCall = (k) -> ko.computed ->
    # Ignore non-CTI users or users not busy with a call action
    if k.currentAType() != window.global.idents("ActionType").call
      return null

    calls = k.lastAvayaSnapshot()?.calls
    return _.find(_.keys(calls), (k) -> !calls[k].held && calls[k].answered)

  mkListenTo = (k) -> () ->
    if k._meta.currentCall()? && window.global.CTIPanel?
      window.global.CTIPanel.bargeIn k._meta.currentCall(), "Silent"

  # True iff our supervisor CTI currently has a call of this user
  mkBeingListened = (k) -> ko.computed ->
    if k._meta.currentCall()? && window.global.CTIPanel?
      _.find(window.global.CTIPanel.calls(),\
        (c) -> c.callId == k._meta.currentCall())

  # Take over the current call of the user, ejecting him
  mkTakeover = (k) -> () ->
    call = k._meta.currentCall()
    if call? && window.global.CTIPanel?
      # Wait for our CTI to connect to the call
      sub = window.global.CTIPanel.calls.subscribe (nv) ->
        if k._meta.beingListened()
          sub.dispose()
          ejectUrl = "/avaya/eject/#{k.userid()}/#{call}"
          $.ajax ejectUrl, {type: "PUT", success: (r) ->
            if r.caseId?
              $.notify "Открываю кейс #{r.caseId}…"
              whereTo = "case/#{r.caseId}"
            else
              $.notify "Открываю звонок #{r.callId}…"
              whereTo = "call/#{r.callId}"
            window.location.hash = whereTo}
      window.global.CTIPanel.bargeIn call, "Active"

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

  template: Tpl()
  constructor: (view, opts) -> fetchJson (Model) ->
    mp = new Map.Mapper(Model)
    update = (kvmsh) -> (data) ->
      for d in data
        do (d) ->
          a = mp.s2cObj d
          kvm = kvmsh[a.userid]
          if kvm?
            for k, v of a
              do (k, v) -> kvm[k](v)
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
            k._meta.beingListened  = mkBeingListened(k)
            k._meta.takeover       = mkTakeover(k)
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
