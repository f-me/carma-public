define [ "model/main"
       , "utils"
       , "text!tpl/screens/back.html"], (Main, utils, tpl) ->
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
    pci = global.idents('ProcessingConfig').main
    pcvm = Main.buildKVM global.model('ProcessingConfig'), {fetched: {id: pci}}
    startCycle pcvm

  startCycle = (pcvm) ->
   if _.contains global.user.roles, global.idents("Role").call
     # COMPLEX LOGIC HERE
   else
     # Only pull actions
     pullActions true

  # Install automatic poller for actions only
  setupActionsPoller = ->
    # Time since last cycle start, in seconds
    current_cycle = 0
    worker = ->
      if onBackofficeScreen
        current_cycle += cycle_resolution
        percent = current_cycle / poll_every * 100.0
        if current_cycle >= poll_every
          pullActions true
          current_cycle = 0
        else
          setTimeout worker, (cycle_resolution * 1000)
    worker()

  # Given /littleMoreActions response, try to redirect to the first
  # action. If onlyActions=true, then actions-only handler is setup
  # afterwards.
  myActionsHandler = (onlyActions) ->
    if !_.isEmpty actions
      act = _.first actions
      openAction act
    else
      pleaseStandby()
      if onlyActions
        setupActionsPoller()
      else
        startCycle()

  # Pull new actions for user
  pullActions = (onlyActions) ->
    $.ajax
      type: "PUT"
      url: "/backoffice/littleMoreActions"
      success: myActionsHandler onlyActions
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
