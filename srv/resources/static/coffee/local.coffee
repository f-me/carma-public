#/ Everything local to the customer resides here

require [ "domready"
        , "model/main"
        , "routes"
        , "hooks/config"
        , "json!/cfg/dictionaries"
        , "json!/_whoami"
        , "json!/cfg/models"
        , "json!/s/screens"
        , "json!/allUsers"
        , "utils"
        , "sendSms"
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
           ) ->

  window.onerror = (msg, url, line) ->
    $.ajax
      type: "POST"
      url : "/errors"
      data: "#{msg} #{url} #{line}"
    return false

  redirectToHomePage = (user) ->
    if _.contains user.roles, "front"
      homePage = "call"
    else if _.contains user.roles, "back"
      homePage = "back"
    else if _.contains user.roles, "ruslan"
      homePage = "contract/1"
    else if _.contains user.roles, "vwpartner"
      homePage = "contract/2"
    else if _.contains user.roles, "supervisor"
      homePage = "supervisor"
    else if _.contains user.roles, "parguy"
      homePage = "partner"
    else if _.contains user.roles, "head"
      homePage = "rkc"
    global.router.navigate homePage, {trigger: true}

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
              models
    global.all_users = users
    global.nav = nav
    global.keys = {}
    global.keys.arrows = {left: 37, up: 38, right: 39, down: 40 }
    ko.applyBindings global.nav, $('#nav')[0]
    ext = user.meta.avayaExt
    pwd = user.meta.avayaPwd
    if ext and pwd
      global.avayaPhone = new AvayaWidget($('#avaya-panel'), ext, pwd)
    if window.location.hash == ""
      redirectToHomePage user

    sendSms.setup()

  u.build_global_fn 'showComplex', ['utils']
  u.build_global_fn 'hideComplex', ['utils']
  u.build_global_fn 'doPick', ['utils']
  u.build_global_fn 'kdoPick', ['utils']
  u.build_global_fn 'focusField', ['utils']
  u.build_global_fn 'uploadFile', ['fileupload']
  u.build_global_fn 'deleteFile', ['fileupload']
