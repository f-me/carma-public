#/ Everything local to the customer resides here

require [ "domready"
        , "model/main"
        , "routes"
        , "hooks/config"
        , "json!/cfg/dictionaries"
        , "json!/_whoami"
        , "json!/_/Usermeta"
        , "utils"
        , "sendSms"
        , "liveMenu"
        , "lib/bug-report"
        , "lstorePubSub"
        , "lib/current-user"
        ], ( dom
           , main
           , Finch
           , hooks
           , dicts
           , user
           , users
           , u
           , sendSms
           , liveMenu
           , bug
           , pubSub
           , CurrentUser
           ) ->

  bugReport = new bug.BugReport

  # collect errors in console to add them to the bug report
  # and then send to server
  do ->
    reportBug = (msg, url, line) ->
      bugReport.addError msg, url, line
      # This can cause dos when backend fail to handle request
      # $.ajax
      #   type: "POST"
      #   url : "/errors"
      #   data: "#{msg} #{url} #{line}"

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
          {value: i.login, label: "#{i.realName} (#{i.login})"}
    dicts.roles =
      entries:
        for i in users
          {value: i.login, label: i.roles }

    main.setup Finch,
              dicts,
              hooks,
              user,
              new pubSub
    global.keys = {}
    global.keys.arrows = {left: 37, up: 38, right: 39, down: 40 }

    liveMenu.setup(document.getElementById 'nav')

    avayaCred = document.cookie.match /avaya=([^;]*)/
    if avayaCred?[1]
      extPwd = unescape(avayaCred[1]).match /(.*)\|(.*)/
      if extPwd
        global.avayaPhone = new AvayaWidget($('#avaya-panel'), extPwd[1], extPwd[2])

    sendSms.setup()

    if user.login == "darya"
      $('#icon-user').removeClass('icon-user').addClass('icon-heart')

    # Enable Popover data API
    $( () -> $('body').popover
                          html: true,
                          selector: '[data-provide="popover"]',
                          trigger: 'hover')

    CurrentUser.initialize()

  u.build_global_fn 'showComplex', ['utils']
  u.build_global_fn 'hideComplex', ['utils']
  u.build_global_fn 'inlineUploadFile', ['lib/upload']
  u.build_global_fn 'inlineDetachFile', ['lib/upload']
  u.build_global_fn 'doPick', ['utils']
  u.build_global_fn 'kdoPick', ['utils']
  u.build_global_fn 'focusField', ['utils']
