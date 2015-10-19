define ["utils"], (u) ->
  handleLeft = (e) ->
    arrs = window.global.keys.arrows
    if e.ctrlKey and e.keyCode == arrs.right
      c = window.global.nav.lastCenter
      if c and c.is(':visible')
        c.focus()
      else if f = $('#center fieldset:visible input')
        f.first().focus()

  handleCenter = (e) ->
    arrs = window.global.keys.arrows
    l = window.global.nav.lastLeft
    if e.ctrlKey and e.keyCode == arrs.left and l
      u.checkAccordion(l)
      l.focus()

  setup: ->
    $('#left').on('keydown.hotkeys', handleLeft)
    $('#center').on('keydown.hotkeys', handleCenter)

    # set focus hystory for hotkey navigation
    window.global.nav = {}
    $('#left'  ).on 'focus.nav', 'input', (e) ->
      window.global.nav.lastLeft = $(e.currentTarget)
    $('#center').on 'focus.nav', 'input', (e) ->
      window.global.nav.lastCenter = $(e.currentTarget)

    # set global hotkeys
    $(document).off 'keydown.closecomp'
    $(document).on  'keydown.closecomp', (e) ->
      # close center on C-m
      hideComplex() if e.ctrlKey and e.keyCode == 77 # m key
      # reload screen
      if e.ctrlKey and e.keyCode == 78
        $("#reload-screen").click()
