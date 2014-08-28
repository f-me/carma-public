define ['json!/screens'], (screens) ->

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

  expand: expand
  shrink: shrink
  reset:  reset

  setup: (domEl) ->
    menuItems = ko.observableArray [moreItem.screens(screens)]
    ko.applyBindings menuItems, domEl
    window.onresize = _.debounce reset, 500
    do expand
