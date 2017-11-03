{$, _} = require "carma/vendor"
{data} = require "carma/data"

user = data.model.user

# Client-side hacker extensions and hidden features
#
# To add a new hack, insert an entry in `hackMap` below and add a
# link of type `hack` in screens.json:
#
#         { "name"  : "permute-case-panes",
#           "label" : "Переставить панели кейса",
#           "type"  : "hack",
#           "permissions": ["hacker"]
#         }
#
# Hack state (on/off) is stored per user. Once activated, hacks are
# re-enabled on every page load. Disabling a hack means turning this
# re-activation off.

addLocalCSSRule = (selector, rule) ->
  styles = document.styleSheets
  localCSS = _.find styles, (s) -> s.title == "local"
  # For some reason insertRule does not work with
  # ::-webkit-scrollbar-thumb selectors
  localCSS.addRule selector, rule

# How to enable hacks.
#
# A hack cannot be explicitly disabled (we reload the page without
# re-enabling the hack instead).
hackMap =
  'permute-case-panes': ->
    addLocalCSSRule "#right", "left: 0; width: 20%;"
    addLocalCSSRule "#left", "left: 22%; width: 30%;"
    addLocalCSSRule "#center", "left: 54%; width: 44%;"

usermetaUrl = -> "/_/Usermeta/#{user.id}"

addHack = (h) ->
  $.getJSON usermetaUrl(), (res) ->
    stuff = res.stuff
    hacks = stuff?.hacks || []
    if not _.contains hacks, h
      hacks.push h
      stuff.hacks = hacks
      user.stuff.hacks = hacks # FIXME WHAT THE FUCK? it was just data!
      $.putJSON(usermetaUrl(), {stuff}).
        done(-> hackMap[h]?())

dropHack = (h) ->
  $.getJSON usermetaUrl(), (res) ->
    stuff = res.stuff
    hacks = _.without (stuff?.hacks || []), h
    stuff.hacks = hacks
    user.stuff.hacks = hacks # FIXME WHAT THE FUCK? it was just data!
    $.putJSON(usermetaUrl(), {stuff}).
      done(-> location.reload())


module.exports =

  # Activate hacks previously enabled by the user
  reenableHacks: ->
    for h in user.stuff?.hacks || []
      console.log "Re-enabling hack #{h}"
      hackMap[h]?()

  switchHack: (link) ->
    hack = $(link).data('hack')

    # Was it previously enabled?
    if not _.contains user.stuff.hacks, hack
      console.log "Switching hack #{hack} ON"
      addHack hack
    else
      console.log "Switching hack #{hack} OFF"
      dropHack hack

    return false
