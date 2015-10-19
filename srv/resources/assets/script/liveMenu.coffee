define [ 'knockout'
       , 'lib/navbar.jade'
       ], (ko, Tpls) ->

  fetchJson = (done) ->
    fetch('/screens', {credentials: 'same-origin'}, {credentials: 'same-origin'})
      .then((resp) -> resp.json())
      .then done

  menuItems = null
  moreItem =
    name:    'more'
    type:    'dropdown'
    label:   'Ещё'
    screens: ko.observableArray()

  weHaveMore = (xs) -> xs[xs.length - 1] == moreItem

  expand = ->
    if weHaveMore menuItems()

      baseHeight = $('.navbar').height()

      menuItems.pop() # temporarily cut 'more' away
      migrantItem = moreItem.screens.shift()
      menuItems.push migrantItem

      # put 'more' at the end if it is not empty
      if moreItem.screens().length > 0
        menuItems.push moreItem

      newHeight = $('.navbar').height()
      if newHeight > baseHeight then do shrink else do expand

  shrink = ->
    if weHaveMore menuItems()
      menuItems.pop()
    migrantItem = menuItems.pop()
    moreItem.screens.unshift migrantItem
    menuItems.push moreItem

  reset = ->
    while menuItems().length > 1
      do shrink
    do expand

  tpls = $('<div/>').append($(Tpls()))
  ko.virtualElements.allowedBindings.renderMenuEl = true
  ko.bindingHandlers.renderMenuEl =
    init: (el, acc) ->
      scr = ko.utils.unwrapObservable acc()
      tpl = tpls.find("##{scr.type}").html()
      ko.virtualElements.prepend(el, $(tpl)[0])

  expand: expand
  shrink: shrink
  reset:  reset

  setup: (domEl) -> fetchJson (screens) ->
    menuItems = ko.observableArray [moreItem.screens(screens)]
    ko.applyBindings menuItems, domEl
    window.onresize = _.debounce reset, 500
    do expand
