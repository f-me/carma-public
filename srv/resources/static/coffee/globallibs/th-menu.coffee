class ThMenu
  constructor: (options) ->
    @options     = $.extend({}, $.fn.typeahead.defaults, options)
    @matcher     = @.options.matcher     || @.matcher
    @sorter      = @.options.sorter      || @.sorter
    @highlighter = @.options.highlighter || @.highlighter
    @$menu       = $(@.options.menu).appendTo('body')

    @selectcb    = options.select
    @dict        = options.dict

    @shown = false

    @$menu
      .on('click.typeahead',            $.proxy(@.click, @))
      .on('mousedown.typeahead',        $.proxy(@.mousedown, @))
      .on('mouseenter.typeahead', 'li', $.proxy(@.mouseenter, @))

  setElement: (el) =>
    @$element = $(el)
    @$element
      .on('blur.typeahead',     $.proxy(@.blur, @))
      .on('keypress.typeahead', $.proxy(@.keypress, @))
      .on('keyup.typeahead',    $.proxy(@.keyup, @))

    if $.browser.webkit or $.browser.msie
      @$element.on('keydown.typeahead', $.proxy(@.keypress, @))

  destructor: =>
    @$menu.remove
    @$element
      .off('blur.typeahead')
      .off('keypress.typeahead')
      .off('keyup.typeahead')

    if $.browser.webkit or $.browser.msie
      @$element.off('keydown.typeahead')

  select: =>
    @selectcb(@.$menu.find('.active').attr('data-value'))
    @hide()
    @$element.change()
    return @

  show: =>
    pos = $.extend {}, @.$element.offset(),
      height: @.$element[0].offsetHeight

    @$menu.css
      top: pos.top + pos.height
      left: pos.left
      minWidth: @.$element.width() + "px"

    @$menu.show()
    @shown = true
    $(document).on('mousedown', $.proxy(@.hide, @))
    return @

  hide: =>
    return @ unless @.shown
    @$menu.hide()
    @shown = false
    $(document).off('mousedown', @.hide)
    return @

  toggle: =>
    if @shown
    then @hide()
    else @show()
    return @

  setItems: @render
  render: (items) =>
    that = @

    itms = $.map items, (v, k) ->
      i = if _.isFunction that.options.item
            that.options.item.call(that, item)
          else
            el = $(that.options.item)
            el.find('a').html(v)
            el
      i.attr('data-value', k)
      i[0]

    $(itms).first().addClass('active')
    @$menu.html(itms)
    return @

  inViewport: (el) =>
    m = @$menu
    elOffset = el.offset().top - m.offset().top
    return 0 < elOffset && elOffset < m.height()

  next: (event) =>
    active = @$menu.find('.active').removeClass('active')
    next   = active.next()

    next = $(@$menu.find('li')[0]) unless next.length

    next.addClass('active')
    @inViewport(next) || next[0].scrollIntoView(false)

  prev: (event) =>
    active = @$menu.find('.active').removeClass('active')
    prev   = active.prev()

    prev = @$menu.find('li').last() unless prev.length

    prev.addClass('active')
    @inViewport(prev) || prev[0].scrollIntoView(false)

  blur: (e) => setTimeout((=> @hide()), 150)

  click: (e) =>
    e.stopPropagation()
    e.preventDefault()
    @select()

  mouseenter: (e) =>
    @.$menu.find('.active').removeClass('active')
    $(e.currentTarget).addClass('active')

  mousedown: (e) -> e.stopPropagation(); e.preventDefault()

  # debounce here will decrease request rate for remote dictionaries
  # still it's small enough to not be noticable for regular dicts
  draw: _.debounce((-> @_draw()), 150)

  _draw:  =>
    return @ unless @$element
    @dict.lookup @$element.val(), (v) =>
      return @.hide() if _.isEmpty v
      return @.render(v).show()

  # this methos is for .add-on arrow on the field
  drawAll: =>
    return @ unless @$element
    @dict.lookup "", (v) =>
      return @.hide() if _.isEmpty v
      return @.render(v).show()


  keyup: (e) =>
    switch e.keyCode
      when 40, 38 # down arrow, up arrow
        @draw() unless @shown
      when 9, 16, 37, 39  # tab, shift, left, right
        return
      when 13     # enter
        @select() if @shown
      when 27     # escape
        @hide()

      else
        @draw()

    e.stopPropagation()
    e.preventDefault()

  keypress: (e) =>
    return unless @shown

    switch e.keyCode
      when 9, 13, 27 # tab, enter, escape
        e.preventDefault()

      when 38        # up arrow
        e.preventDefault()
        @prev()

      when 40        # down arrow
        e.preventDefault()
        @next()

    e.stopPropagation()
