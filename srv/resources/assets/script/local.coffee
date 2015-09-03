#/ Everything local to the customer resides here

require [ "domready"
        , "model/main"
        , "routes"
        , "hooks/config"
        , "json!/cfg/dictionaries"
        , "json!/_whoami"
        , "json!/_/Usermeta"
        , "utils"
        , "sync/crud"
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
           , sync
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
    sendError = (err) ->
      if not err.match(/whoopsie/)
        $.ajax
          type: 'POST'
          url : '/whoopsie'
          data: err

    window.onerror = (msg, url, line, pos, err) ->
      bugReport.addError msg, url, line
      sendError err.stack

    # FIXME: report websosket errors also
    $(document).ajaxError (event, jqXHR, s, error) ->
      sendError [
          s.type, s.url, jqXHR.status,
          jqXHR.statusText, s.data, jqXHR.responseText
      ].join('  ')

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
      # Setup CTI panel
      if _.contains user.roles, global.idents("Role").cti
        if user.workPhoneSuffix.match(/^\d+$/)
          cti = new CTI user.workPhoneSuffix
          vips = u.newModelDict("VipNumber", false, {dictionaryLabel: 'number'})
          vdns = u.newModelDict("VDN", false, {dictionaryLabel: 'number'})
          opts =
            # AVAYA halts when this is dialed
            bannedNumbers: ["8"]
            displayedToInternal: u.displayedToInternal
            internalToDisplayed: u.internalToDisplayed
            isVipCb: (n) -> vips.getVal(n)
            vdnToDisplayed:
              (vdnNumber) ->
                vdnNumber = vdnNumber?.split(":")[0]
                vdn = vdns.getElement(vdns.getVal(vdnNumber))
                if vdn?
                  "#{vdn?.label}: #{vdn?.greeting}"
                else
                  null
            onexagentPort: 60000
            # Fill caller phone and program when answering a call on
            # call screen
            answerCallCb: (number, vdnNumber) ->
              if _.contains global.user.roles, global.idents("Role").call
                number = u.internalToDisplayed number
                if number.length > 5
                  vdnNumber = vdnNumber?.split(":")[0]
                  vdn = vdns.getElement(vdns.getVal(vdnNumber))
                  callData = {}
                  if number?
                    callData.callerPhone = number
                  else
                    callData.callerPhone = ""
                  if vdn?.program
                    callData.program = vdn.program
                  u.createNewCall callData
                  localStorage["call.search-query"] = "!Тел:" + number
            incomingCallCb: -> $("#cti").show()
          global.CTIPanel = new CTIPanel cti, $("#cti"), opts
          Mousetrap.bind ["`", "ё"], () ->
            $("#cti").toggle()
          Mousetrap.bind "ctrl+enter", () -> global.CTIPanel.answer()
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
