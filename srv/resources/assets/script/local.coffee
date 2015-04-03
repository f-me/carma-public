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
        , "lib/hacking"
        , "lib/cti"
        , "lib/cti-panel"
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
           , hacking
           , CTI
           , CTIPanel
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

    # Cached mapping between from userid to "name (login)"
    dicts.users =
      entries:
        for i in users
          {value: String(i.id), label: "#{i.realName} (#{i.login})"}
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

    hacking.reenableHacks()

    # disable everytnig websocket-related for portal
    if not window.location.origin.match(/portal\.ruamc\.ru/)
      # New CTI panel
      if _.contains user.roles, global.idents("Role").cti
        if user.workPhoneSuffix.match(/^\d+$/)
          cti = new CTI user.workPhoneSuffix
          vips = u.newModelDict("VipNumber", false, {dictionaryLabel: 'number'})
          opts =
            # AVAYA halts when this is dialed
            bannedNumbers: ["8"]
            displayedToInternal:
              (number) ->
                number.replace("+7", "98").replace("+", "9810")
            internalToDisplayed:
              (number) ->
                number?.match(/\d+/)?[0]?.replace(/^(98|8|)(\d{10})$/, "\+7$2")
            isVipCb: (n) -> vips.getVal(n)
          global.CTIPanel = new CTIPanel cti, $("#cti"), opts
        else
          console.error "Malformed workPhoneSuffix \"#{user.workPhoneSuffix}\""

    sendSms.setup()

    if user.login == "darya"
      $('#icon-user').removeClass('icon-user').addClass('icon-heart')

    # Enable Popover data API
    $( () -> $('body').popover
                          html: true,
                          selector: '[data-provide="popover"]',
                          trigger: 'hover')

    # disable everytnig websocket-related for portal
    if not window.location.origin.match(/portal\.ruamc\.ru/)
      CurrentUser.initialize()

    # render menu only after everything else in menu bar is done
    liveMenu.setup(document.getElementById 'nav')

    # file field selection (currenlty only on vin screen)
    $(document).on 'change', '.btn-file :file', ->
      input = $(this)
      numFiles = if input.get(0).files then input.get(0).files.length else 1
      label = input.val().replace(/\\/g, '/').replace(/.*\//, '')
      textInput = $(this).parents('.input-group').find(':text')
      textInput.val(label)

  u.build_global_fn 'switchHack', ['lib/hacking']
  u.build_global_fn 'showComplex', ['utils']
  u.build_global_fn 'hideComplex', ['utils']
  u.build_global_fn 'inlineUploadFile', ['lib/upload']
  u.build_global_fn 'inlineDetachFile', ['lib/upload']
  u.build_global_fn 'doPick', ['utils']
  u.build_global_fn 'kdoPick', ['utils']
  u.build_global_fn 'edoPick', ['utils']
  u.build_global_fn 'focusField', ['utils']
  u.build_global_fn 'ctiDial', ['utils']
