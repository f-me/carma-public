define [], ->
  # Remove all content of view and clean up wares.
  #
  # To setup view back again, call
  # screen.views[viewName]($el(viewName), args);
  forgetView = (viewName) ->
    vW = window.global.viewsWare[viewName]
    # View may have not setup any knockVM (static views like search)
    # FIXME: do we have to release knockvm at all?
    # if not _.isUndefined(vW.knockVM) then kb.vmRelease(vW.knockVM)
    vW = {}
    $el(viewName).empty()

  # Clean up all views on screen and everything.
  forgetScreen = ->
    for name, cs of window.global.activeScreen?.views when cs.destructor?
      cs.destructor(name)
    forgetView(viewName) for viewName of window.global.viewsWare
    window.global.topElement.off()
    ko.cleanNode window.global.topElement[0]
    window.global.topElement.empty()
    window.global.viewsWare = {}
    window.global.activeScreen = null


  # Render top-level screen template (static)
  #
  # args object is passed further to all view setup functions.
  renderScreen: (screenObj, args) ->
    forgetScreen()
    screen = screenObj.screen
    screen.reload = => @renderScreen(screenObj, args)
    window.global.activeScreen = screen

    # Highlight the new item in navbar
    $("li.active").removeClass("active")
    $el(screen.name + "-screen-nav").addClass("active")

    rawPartials = if screenObj.partials
        $('.partial').add($(screenObj.partials).siblings(".partial"))
      else
        $('.partial')

    partials = {}
    for p in rawPartials
      partials[$(p).attr('id')] = $(p).html()

    wrappers = screenObj.wrappers?(partials) or {}

    tpl   = screenObj?.template
    tpl ||= $el(screen.template).html()

    unless tpl?
      throw "Template for screen #{screen.name} is not found"

    tpl1 = Mustache.render tpl, wrappers, partials
    window.global.topElement.html(tpl1)
    # Call setup functions for all views, assuming they will set
    # their viewsWare
    for viewName, cs of screen.views when cs.constructor?
      cs.constructor(viewName, args)

  # Clean up all views on screen and everything.
  forgetScreen: forgetScreen
