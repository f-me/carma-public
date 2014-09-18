define [], ->
  # Client-side hacker extensions and hidden features

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
    'bright-new-world': ->
      addLocalCSSRule "body", "background-color: #f7f7f7;"
      addLocalCSSRule(
        ".nice-scrollbar::-webkit-scrollbar-thumb:vertical",
        "background-color: #eaeaea;")

    'permute-case-panes': ->
      addLocalCSSRule "#right", "left: 0; width: 20%;"
      addLocalCSSRule "#left", "left: 22%; width: 30%;"
      addLocalCSSRule "#center", "left: 54%; width: 44%;"

  usermetaUrl = -> "/_/Usermeta/#{global.user.id}"

  addHack = (h) ->
    $.getJSON usermetaUrl(), (res) ->
      stuff = res.stuff
      hacks = stuff?.hacks || []
      if not _.contains hacks, h
        hacks.push h
        stuff.hacks = hacks
        global.user.stuff.hacks = hacks
        $.putJSON(usermetaUrl(), {stuff: stuff}).
          done(-> hackMap[h]?())

  dropHack = (h) ->
    $.getJSON usermetaUrl(), (res) ->
      stuff = res.stuff
      hacks = _.without (stuff?.hacks || []), h
      stuff.hacks = hacks
      global.user.stuff.hacks = hacks
      $.putJSON(usermetaUrl(), {stuff: stuff}).
        done(-> location.reload())

  # Activate hacks previously enabled by the user
  reenableHacks: ->
    for h in global.user.stuff?.hacks || []
      console.log "Re-enabling hack #{h}"
      hackMap[h]?()

  switchHack: (link) ->
    hack = $(link).data('hack')

    # Was it previously enabled?
    if not _.contains global.user.stuff.hacks, hack
      console.log "Switching hack #{hack} ON"
      addHack hack
    else
      console.log "Switching hack #{hack} OFF"
      dropHack hack

    return false
