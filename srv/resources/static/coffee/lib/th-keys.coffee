class ThKeys
  constructor: (@menu, @dict) ->
    @menu.$element
      .on('blur.typeahead',     $.proxy(@.blur, @))
      .on('keypress.typeahead', $.proxy(@.keypress, @))
      .on('keyup.typeahead',    $.proxy(@.keyup, @))

    if $.browser.webkit or $.browser.msie
      @menu.$element.on('keydown.typeahead', $.proxy(@.keypress, @))

  destructor: ->
    @menu.$element
      .off('blur.typeahead')
      .off('keypress.typeahead')
      .off('keyup.typeahead')

    if $.browser.webkit or $.browser.msie
      @menu.$element.off('keydown.typeahead')

  keyup: (e) ->
    switch e.keyCode
      when 40, 38 # down arrow, up arrow
        @menu.show() unless @menu.shown
      when 9, 16  # tab, shift
        return
      when 13     # enter
        if @menu.shown
        then @menu.select()
        else @menu.show()

      when 27     # escape
        @menu.hide()

      else
        l = @dict.lookup(@menu.$element.val())
        @menu.draw(l)

    e.stopPropagation()
    e.preventDefault()

  keypress: (e) ->
    return unless @menu.shown

    switch e.keyCode
      when 9, 13, 27 # tab, enter, escape
        e.preventDefault()

      when 38        # up arrow
        e.preventDefault()
        @menu.prev()

      when 40        # down arrow
        e.preventDefault()
        @menu.next()

    e.stopPropagation()
