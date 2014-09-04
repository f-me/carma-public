define [ "utils"
       , "text!tpl/screens/back.html"], (utils, tpl) ->
  onBackofficeScreen = true

  # In s
  cycle_resolution = 0.1

  # Poll server every n seconds
  poll_every = 5

  pleaseStandby = ->
    $("#standby-msg").fadeOut('fast').fadeIn('fast')

  setupBackOffice = ->
    onBackofficeScreen = true
    $("#zardoz").knob()
    setTimeout (-> setupPoller()), 200

  # Install automatic actions poller
  setupPoller = ->
    # Time since last cycle start, in seconds
    current_cycle = 0
    worker = ->
      if onBackofficeScreen
        current_cycle += cycle_resolution
        percent = current_cycle / poll_every * 100.0
        $("#zardoz").val(percent).trigger('change')
        if current_cycle >= poll_every
          pullActions()
          current_cycle = 0
        else
          setTimeout worker, (cycle_resolution * 1000)
    worker()

  # Given /littleMoreActions response, try to redirect to the first
  # action
  myActionsHandler = (actions) ->
    if !_.isEmpty actions
      openCaseAction act.id, act.caseId
    else
      pleaseStandby()
      setupPoller()

  # Pull new actions for user
  pullActions = ->
    $.ajax
      type: "PUT"
      url: "/backoffice/littleMoreActions"
      success: myActionsHandler

  removeBackOffice = ->
    # Stop auto-polling backoffice-related server handlers when we
    # leave #back
    onBackofficeScreen = false

  # Start working on an action and redirect to its case
  openCaseAction = (actId, caseId) ->
    $.ajax
      type: "PUT"
      url: "/backoffice/openAction/#{actId}"
      success: () -> window.location.hash = "case/#{caseId}"

  { constructor: setupBackOffice
  , destructor: removeBackOffice
  , template: tpl
  }
