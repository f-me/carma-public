#/ Everything local to the customer resides here

require [ "domready"
        , "model/main"
        , "routes"
        , "hooks/config"
        , "json!/cfg/dictionaries"
        , "json!/_whoami"
        , "json!/screens"
        , "json!/allUsers"
        , "utils"
        , "sendSms"
        , "lib/bug-report"
        , "lstorePubSub"
        ], ( dom
           , main
           , Routes
           , hooks
           , dicts
           , user
           , nav
           , users
           , u
           , sendSms
           , bug
           , pubSub
           ) ->

  bugReport = new bug.BugReport

  # collect errors in console to add them to the bug report
  # and then send to server
  do ->
    reportBug = (msg, url, line) ->
      bugReport.addError msg, url, line
      $.ajax
        type: "POST"
        url : "/errors"
        data: "#{msg} #{url} #{line}"

    originConsoleError = console.error

    console.error = (msg, url, line) ->
      reportBug msg, url, line
      originConsoleError.apply console, [msg, url, line]

    window.onerror = console.error

    $(document).ajaxError (event, jqXHR, settings, error) ->
      console.error "#{settings.type} #{settings.url} #{jqXHR.status}
 (#{jqXHR.statusText})\n#{settings.data}\n#{jqXHR.responseText}"

  # this will be called on dom ready
  dom ->
    bugReport.setElement $('#send-bug-report')

    dicts.users =
      entries:
        for i in users
          {value: i.value, label: "#{i.label} (#{i.value})"}
    dicts.roles =
      entries:
        for i in users
          {value: i.value, label: i.roles }

    main.setup Routes.localScreens(),
              Routes.localRouter,
              dicts,
              hooks,
              user,
              new pubSub
    global.all_users = users
    global.nav = nav
    global.keys = {}
    global.keys.arrows = {left: 37, up: 38, right: 39, down: 40 }
    ko.applyBindings global.nav, $('#nav')[0]

    avayaCred = document.cookie.match /avaya=([^;]*)/
    if avayaCred?[1]
      extPwd = unescape(avayaCred[1]).match /(.*)\|(.*)/
      if extPwd
        global.avayaPhone = new AvayaWidget($('#avaya-panel'), extPwd[1], extPwd[2])

    if window.location.hash == "" and user.meta.homepage
      global.router.navigate user.meta.homepage, {trigger: true}

    sendSms.setup()

    if user.login == "darya" or user.login == "e.balabanova"
      $('#icon-user').removeClass('icon-user').addClass('icon-heart')


  u.build_global_fn 'showComplex', ['utils']
  u.build_global_fn 'hideComplex', ['utils']
  u.build_global_fn 'doPick', ['utils']
  u.build_global_fn 'kdoPick', ['utils']
  u.build_global_fn 'focusField', ['utils']
