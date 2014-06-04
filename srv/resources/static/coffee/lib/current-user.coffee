# All current user's specific code should be here except legacy
# 'global.user' initialization

define ["model/main", "sync/crud", "lib/messenger" ], (Main, Crud, Messenger)->

  # block other than rest screens when in rest
  oldNav = Finch.navigate
  Finch.navigate = (args...) ->
    st = window.global?.Usermeta?.currentState?()
    if _.contains(["Dinner", "Rest"], st)
      oldNav("rest")
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

    usr.toggleDelayed = (st) =>
      if usr.delayedState() == st
        usr.delayedState null
      else
        usr.delayedState(st)

    Messenger.subscribe "#{global.model('Usermeta').name}:#{usr.id()}",
      usr._meta.q.saveSuccessCb(_.identity)

    usr.currentState?.subscribe (v) =>
      # order of conditions is matter, second won't be avaluatd if first
      # is false, and 'Finch.navigate()' may change current route
      if Finch.navigate() == 'rest' and v == 'Ready'
        window.location.href = user.meta.homepage

    ko.applyBindings(usr, $("#current-user")[0])
    # little hack so dropdown with delayed states won't close when user
    # change next state
    $('#current-user .dropdown-menu').click (event) -> event.stopPropagation()

    if window.location.hash == "" and user.meta.homepage
      Finch.navigate user.meta.homepage.replace '/', ''

