# All current user's specific code should be here except legacy
# 'global.user' initialization

define [ "model/main"
       , "sync/crud"
       , "lib/messenger"
       , "sync/datamap" ], (Main, Crud, Messenger, Map)->

  # block other than rest screens when in rest
  oldNav = Finch.navigate
  Finch.navigate = (args...) ->
    st = window.global?.Usermeta?.currentState?()
    if _.contains(["Dinner", "Rest"], st)
      oldNav("rest")
    else if st == 'ServiceBreak'
      oldNav('serviceBreak')
    else if st == 'NA'
      oldNav('na')
    else
      oldNav.apply(@, args)

  checkStuff = ->
    um = window.global.Usermeta
    if _.isUndefined um
      throw Error("current user is undefined, initialize it first")
    if _.isUndefined um.stuff
      throw Error("need permission to read 'usermeta.stuff'")

  initialize: =>
    user = window.global.user

    Role = global.idents 'Role'
    homepage = ""
    homepage = "/#partner"    if _.contains user.roles, Role.parguy
    homepage = "/#back"       if _.contains user.roles, Role.back
    homepage = "/#call"       if _.contains user.roles, Role.call
    homepage = "/#supervisor" if _.contains user.roles, Role.supervisor
    homepage = "/#rkc"        if _.contains user.roles, Role.head


    usr = Main.buildKVM global.model('Usermeta'),
      queue: Crud.CrudQueue
      fetched: {id: user.id}

    # keep current user kvm inside global, can't do this in 'model/main'
    # due to dependencies
    window.global.Usermeta = usr

    Messenger.subscribe "#{global.model('Usermeta').name}:#{usr.id()}",
      usr._meta.q.saveSuccessCb(_.identity)

    usr.currentState?.subscribe (v) =>
      # order of conditions is matter, second won't be avaluatd if first
      # is false, and 'Finch.navigate()' may change current route
      if (_.contains ['rest', 'serviceBreak', 'na'], Finch.navigate()) and
          v == 'Ready'
        window.location.href = homepage

    usr.timeInCurrentState = ko.observable()

    # calculate diff between state change and current time in 'hh:mm' format
    calcTime = =>
      return unless usr.currentStateCTime?()
      t1 = Date.parseExact usr.currentStateCTime(), Map.guiUTCTimeFormat
      t2 = new Date()
      diff = t2 - t1
      msec = diff
      hh = Math.floor(msec / 1000 / 60 / 60)
      msec -= hh * 1000 * 60 * 60
      mm = Math.floor(msec / 1000 / 60)
      msec -= mm * 1000 * 60
      ss = Math.floor(msec / 1000)
      msec -= ss * 1000
      mmpretty = if mm < 10 then "0#{mm}" else mm
      hhpretty = if hh < 10 then "0#{hh}" else hh
      usr.timeInCurrentState "#{hhpretty}:#{mmpretty}"

    # update time diff on each state change
    usr.currentStateCTime?.subscribe (v) => calcTime()

    # update time diff each 10 seconds
    setInterval(calcTime, 10000)

    usr.delayedStateLocal?.subscribeWithOld (n, o) =>
      return if usr.currentState?() == "Ready"
      return if _.isNull n
      msg = "Переход в статус \"#{n}\" после завершения текущего действия."
      $.notify msg, className: "info"

    ko.applyBindings(usr, $("#current-user")[0])
    # little hack so dropdown with delayed states won't close when user
    # change next state
    # $('#current-user .dropdown-menu').click (event) -> event.stopPropagation()

    if window.location.hash == "" and homepage
      Finch.navigate homepage.replace '/', ''

  readStuff: (key) ->
    checkStuff()
    window.global.Usermeta.stuff()[key]

  writeStuff: (key, val) ->
    checkStuff()
    s = window.global.Usermeta.stuff
    newVal = {}; newVal[key] = val
    # write deep copy of previous value, otherwise we can change values that was
    # read by somebody else
    s($.extend(true, {}, s(), newVal))
