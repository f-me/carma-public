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
    else
      oldNav.apply(@, args)

  initialize: =>
    user = window.global.user

    usr = Main.buildKVM global.model('Usermeta'),
      queue: Crud.CrudQueue
      fetched: { id: user.meta.mid }

    # keep current user kvm inside global, can't do this in 'model/main'
    # due to dependencies
    window.global.Usermeta = usr

    Messenger.subscribe "#{global.model('Usermeta').name}:#{usr.id()}",
      usr._meta.q.saveSuccessCb(_.identity)

    usr.currentState?.subscribe (v) =>
      # order of conditions is matter, second won't be avaluatd if first
      # is false, and 'Finch.navigate()' may change current route
      if Finch.navigate() == 'rest' and v == 'Ready'
        window.location.href = user.meta.homepage

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
      if _.isNull n
        msg = "Переход в статус \"#{o}\" отменен."
      else
        msg = "Переход в статус \"#{n}\" после завершения текущего действия."
      $.notify msg, className: "info"

    ko.applyBindings(usr, $("#current-user")[0])
    # little hack so dropdown with delayed states won't close when user
    # change next state
    # $('#current-user .dropdown-menu').click (event) -> event.stopPropagation()

    if window.location.hash == "" and user.meta.homepage
      Finch.navigate user.meta.homepage.replace '/', ''

