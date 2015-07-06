define [ "model/main"
       , "utils"
       , "sync/crud"
       , "text!tpl/screens/back.html"], (Main, utils, sync, tpl) ->
  onBackofficeScreen = true

  # In s
  cycle_resolution = 0.1

  # Poll server for new actions every n seconds
  poll_every = 5

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
    if !_.contains(global.user.roles, global.idents("Role").call)
      $("#new-call-button").hide()
    else
      $("#new-call-button").click () ->
        $("#new-call-button > button").hide()
        if not global.CTIPanel?.answer()
          utils.createNewCall()

    initCycle = () ->
      pci = global.idents('ProcessingConfig').main
      pcvm = Main.buildKVM global.model('ProcessingConfig'),
        {fetched: {id: pci}, queue: sync.CrudQueue}
      startCycle pcvm, false

    # Check for already assigned actions first no matter what the
    # global priority is
    $.ajax
      type: "GET"
      url: "/backoffice/myActions"
      success: myActionsHandler initCycle
      error: initCycle

  startCycle = (pcvm, alternate) ->
    cti = _.contains(global.user.roles, global.idents("Role").cti)
    if cti && _.contains global.user.roles, global.idents("Role").call
      actionsAfterCall = () ->
        return unless onBackofficeScreen
        # If there's an incoming call, postpone actions pulling until
        # we leave the screen
        if global.CTIPanel?.incomingCall()?
          setTimeout actionsAfterCall, 3000
          return
        actuallyPull = () ->
          $("#standby-msg").text "Проверяю наличие действий…"
          pullActions () -> startCycle pcvm, !alternate
        $("#standby-msg").text "Запрещаю приём звонков через AVAYA…"
        # Avoid switching agent state when working without CTI
        if cti
          $.ajax "/avaya/toAfterCall", {type: "PUT", success: actuallyPull}
        else
          actuallyPull()
      # Actions are checked for non-backoffice users as well (so that
      # unfinished call actions are re-opened)
      if (pcvm.actionsFirst() && !alternate)
        actionsAfterCall()
      else
        $("#standby-msg").text "Разрешаю приём звонков через AVAYA…"
        $.ajax "/avaya/toReady", {type: "PUT", success: () ->
          secs = pcvm.callWaitSeconds()
          $("#standby-msg").text "Ожидаю звонки в течение #{secs}с…"
          setTimeout((() -> startCycle(pcvm, !alternate)), secs * 1000)}
    # Non-Front Office users or non-CTI users always pull for actions
    else
      # Only pull actions
      pullActions setupActionsPoller

  # Install automatic poller for actions only
  setupActionsPoller = ->
    # Time since last cycle start, in seconds
    current_cycle = 0
    worker = ->
      if onBackofficeScreen
        current_cycle += cycle_resolution
        percent = current_cycle / poll_every * 100.0
        if current_cycle >= poll_every
          pullActions setupActionsPoller
          current_cycle = 0
        else
          setTimeout worker, (cycle_resolution * 1000)
    worker()

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
          $("#standby-msg").text "Нельзя получить новое действие в статусе «Занят»!"
        else
          $("#standby-msg").text "Что-то пошло не так!"

  removeBackOffice = ->
    # Stop auto-polling backoffice-related server handlers when we
    # leave #back
    onBackofficeScreen = false

  # Start working on an action and redirect to its case. Argument is
  # an element of /littleMoreActions response.
  openAction = (act) ->
    $("#new-call-button").hide()
    if act.caseId?
      $("#standby-msg").text "Открываю действие #{act.id} в кейсе #{act.caseId}…"
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
  , destructor: removeBackOffice
  , template: tpl
  }
