{_: {debounce}} = require "carma/vendor"
{store} = require "carma/neoComponents/store"
actions = require "carma/neoComponents/store/navbar/actions"

# It would be better do this shrinking different way, without hacks like this,
# but while we don't have better solution for this, that what we have to deal
# with.
initHook = (el) ->
  parent = el.parentNode
  initHeight = parent.offsetHeight

  observer = ->
    h = parent.offsetHeight

    # Some safe gap of 5 pixels.
    # For some reason Chromium adds 1 additional pixel to height of the wrapper.
    if h > 0 and Math.abs(initHeight - h) > 5
      store.dispatch actions.hide new actions.hide.Payload

  new MutationObserver(observer).observe el, childList: on

  resizeListener = ->
    store.dispatch actions.resetHidden()
    do observer

  window.addEventListener "resize", debounce(resizeListener, 500), false

module.exports = {initHook}
