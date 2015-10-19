define [ "model/main"
       , "utils"
       , "sync/crud"
       , "screens/back.jade"], (Main, utils, sync, tpl) ->
  onBackofficeScreen = true

  pleaseStandby = ->
    $("#standby-msg").fadeOut('fast').fadeIn('fast')

  setupBackOffice = ->
    onBackofficeScreen = true

    $("#zardoz").height($(window).height() - $("#zardoz").offset().top*2)

    $("#zardoz").spin
      width: 50,
      lines: 15,
      radius: 175

    # Allow manual call creation only for Front Office operators
    if !_.contains(window.global.user.roles, global.idents("Role").call)
      $("#new-call-button").hide()
    else
      $("#new-call-button").click () ->
        $("#new-call-button > button").hide()
        if not window.global.CTIPanel?.answer()
          utils.createNewCall()

    # Setup a new action polling loop or actions/calls alternation
    initCycle = () ->
      cti = _.contains(window.global.user.roles, window.global.idents("Role").cti)
      if cti && _.contains window.global.user.roles, window.global.idents("Role").call
        pci = window.global.idents('ProcessingConfig').main
        pcvm = Main.buildKVM window.global.model('ProcessingConfig'),
          {fetched: {id: pci}, queue: sync.CrudQueue}
        # Start a new alternation cycle depending on global processing
        # settings
        if pcvm.actionsFirst()
          # Actions are checked for non-backoffice users as well (so
          # that unfinished call actions are re-opened)
          actionsAfterCall pcvm
        else
          waitForCall pcvm
      # Non-Front Office users or non-CTI users always pull for new
      # actions
      else
        pullActions setupPlainActionsPoller

    # But first, check for already assigned actions no matter what the
    # global priority is
    $.ajax
      type: "GET"
      url: "/backoffice/myActions"
      success: myActionsHandler initCycle
      error: initCycle

  # Wait for calls, then check new actions (FO+CTI users only)
  waitForCall = (pcvm) ->
    return unless onBackofficeScreen
    $("#standby-msg").text "Разрешаю приём звонков через AVAYA…"
    $.ajax "/avaya/toReady", {type: "PUT", success: () ->
      secs = pcvm.callWaitSeconds()
      $("#standby-msg").text "Ожидаю звонки в течение #{secs}с…"
      setTimeout((() -> actionsAfterCall(pcvm, cti)), secs * 1000)}

  # Check new actions, then wait for calls (FO+CTI users only)
  actionsAfterCall = (pcvm) ->
    return unless onBackofficeScreen
    # If there's an incoming call, postpone actions pulling for 3s
    # (until we leave the screen or the call is dropped)
    if window.global.CTIPanel?.incomingCall()?
      setTimeout actionsAfterCall, 3000
      return
    actuallyPull = () ->
      return unless onBackofficeScreen
      $("#standby-msg").text "Проверяю наличие действий…"
      pullActions () -> waitForCall pcvm
    $("#standby-msg").text "Запрещаю приём звонков через AVAYA…"
    $.ajax "/avaya/toAfterCall", {type: "PUT", success: actuallyPull}

  # Install automatic poller for new actions only (for non-FO+CTI
  # users)
  setupPlainActionsPoller = ->
    # Static polling timeout
    poll_every = 5
    worker = ->
      if onBackofficeScreen
        pullActions setupPlainActionsPoller
    setTimeout worker, poll_every * 1000

  # Given /myActions or /littleMoreActions response, try to redirect
  # to the first action. If the response is empty, call
  # noActionsHandler
  myActionsHandler = (noActionsHandler) -> (actions) ->
    if !_.isEmpty actions
      act = _.first actions
      openAction act
    else
      pleaseStandby()
      noActionsHandler()

  # Pull new actions for user
  pullActions = (noActionsHandler) ->
    $.ajax
      type: "PUT"
      url: "/backoffice/littleMoreActions"
      success: myActionsHandler noActionsHandler
      error: (res) ->
        if res.responseText.match /in non-Ready state/
          $("#standby-msg").text(
            "Нельзя получить новое действие в статусе «Занят»!")
        else
          $("#standby-msg").text "Что-то пошло не так!"

  # Stop auto-polling backoffice-related server handlers when we leave
  # #back
  leaveBackOffice = ->
    onBackofficeScreen = false

  # Start working on an action and redirect to its case. Argument is
  # an element of /littleMoreActions response.
  openAction = (act) ->
    return unless onBackofficeScreen
    $("#new-call-button").hide()
    if act.caseId?
      $("#standby-msg").text(
        "Открываю действие #{act.id} в кейсе #{act.caseId}…")
      whereTo = "case/#{act.caseId}"
    else
      $("#standby-msg").text "Перехожу на звонок #{act.callId}…"
      whereTo = "call/#{act.callId}"
    $.ajax
      type: "PUT"
      url: "/backoffice/openAction/#{act.id}"
      success: () ->
        window.location.hash = whereTo

  { constructor: setupBackOffice
  , destructor: leaveBackOffice
  , template: tpl()
  }
