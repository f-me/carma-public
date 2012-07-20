this.setupHotkeys = ->
  $('#left').on('keydown.hotkeys', handleLeftHotkey)
  $('#center').on('keydown.hotkeys', handleCenterHotkey)

  # set focus hystory for hotkey navigation
  global.nav = {}
  $('#left'  ).on 'focus.nav', 'input', (e) ->
    global.nav.lastLeft = $(e.currentTarget)
  $('#center').on 'focus.nav', 'input', (e) ->
    global.nav.lastCenter = $(e.currentTarget)

  # set global hotkeys
  $(document).off 'keydown.closecomp'
  $(document).on  'keydown.closecomp', (e) ->
    # close center on C-m
    hideComplex() if e.ctrlKey and e.keyCode == 77 # m key

this.handleLeftHotkey = (e) ->
  arrs = global.keys.arrows
  if e.ctrlKey and e.keyCode == arrs.right
    c = global.nav.lastCenter
    if c and c.is(':visible')
      c.focus()
    else if f = $('#center fieldset:visible input')
      f.first().focus()

this.handleCenterHotkey = (e) ->
  arrs = global.keys.arrows
  l = global.nav.lastLeft
  if e.ctrlKey and e.keyCode == arrs.left and l
    checkAccordion(l)
    l.focus()
