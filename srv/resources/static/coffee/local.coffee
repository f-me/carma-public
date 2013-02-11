#/ Everything local to the customer resides here

require [ "domready"
        , "main"
        , "routes"
        , "hooks/config"
        , "json!/cfg/dictionaries"
        , "json!/_whoami"
        , "json!/cfg/models"
        , "json!/s/screens"
        , "json!/usersDict"
        , "utils"
        ], (dom, main, Routes, hooks, dicts, user, models, nav, users, u) ->

  filterScreenPerms = (nav) ->
    nav.screens = fScrnPerms(nav)
    return nav

  fScrnPerms = (nav) ->
    p = global.user.roles
    nav.screens =
      for s in nav.screens when not _.isEmpty _.intersection(s.permissions, p)
        s.screens = fScrnPerms(s) if s.screens
        s
    return nav.screens

  window.onerror = (msg, url, line) ->
    $.ajax
      type: "POST"
      url : "/errors"
      data: "#{msg} #{url} #{line}"
    return false

  redirectToHomePage = (user) ->
    mainRole = user.roles[0]
    if mainRole == "front"
      homePage = "call"
    else if mainRole == "back"
      homePage = "back"
    global.router.navigate(homePage, {trigger: true})

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
    global.nav = filterScreenPerms nav
    global.keys = {}
    global.keys.arrows = {left: 37, up: 38, right: 39, down: 40 }
    ko.applyBindings global.nav, $('#nav')[0]
    ext = user.meta.avayaExt
    pwd = user.meta.avayaPwd
    if ext and pwd
      global.avayaPhone = new AvayaWidget($('#avaya-panel'), ext, pwd)
    if window.location.hash == ""
      redirectToHomePage user

  u.build_global_fn 'showComplex', ['utils']
  u.build_global_fn 'hideComplex', ['utils']
  u.build_global_fn 'doPick', ['utils']
  u.build_global_fn 'kdoPick', ['utils']
  u.build_global_fn 'focusField', ['utils']
  u.build_global_fn 'uploadFile', ['fileupload']
  u.build_global_fn 'deleteFile', ['fileupload']
