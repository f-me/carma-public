#/ Everything local to the customer resides here

require [ "domready"
        , "model/main"
        , "routes"
        , "hooks/config"
        , "json!/cfg/dictionaries"
        , "json!/_whoami"
        , "json!/cfg/models"
        , "json!/screens"
        , "json!/allUsers"
        , "utils"
        , "sendSms"
        , "lstorePubSub"
        ], ( dom
           , main
           , Routes
           , hooks
           , dicts
           , user
           , models
           , nav
           , users
           , u
           , sendSms
           , pubSub
           ) ->

  window.onerror = (msg, url, line) ->
    $.ajax
      type: "POST"
      url : "/errors"
      data: "#{msg} #{url} #{line}"
    return false

  # this will be called on dom ready
  dom ->
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
              models,
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

    # achievements
    if user.meta.achievements.length
      achv = $('#navbar-achievements')
      achvLst = $('#navbar-achievements ul')
      achTpl = (txt) ->
        '<li class="disabled"><a><i class="icon-star icon"/>' +
          '&nbsp;&nbsp;' + txt +
          '</a></li>'
      for a in user.meta.achievements
        achvLst.append achTpl a
      achv.show()


  u.build_global_fn 'showComplex', ['utils']
  u.build_global_fn 'hideComplex', ['utils']
  u.build_global_fn 'doPick', ['utils']
  u.build_global_fn 'kdoPick', ['utils']
  u.build_global_fn 'focusField', ['utils']
