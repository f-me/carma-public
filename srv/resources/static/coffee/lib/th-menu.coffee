class ThMenu
  constructor: (element, options) ->
    @$element    = $(element)
    @options     = $.extend({}, $.fn.typeahead.defaults, options)
    @matcher     = @.options.matcher || @.matcher
    @sorter      = @.options.sorter || @.sorter
    @highlighter = @.options.highlighter || @.highlighter
    @$menu       = $(@.options.menu).appendTo('body')

    @selectcb    = options.select

    @shown = false

    @$menu
      .on('click.typeahead',            $.proxy(@.click, @))
      .on('mousedown.typeahead',        $.proxy(@.mousedown, @))
      .on('mouseenter.typeahead', 'li', $.proxy(@.mouseenter, @))

  destructor: -> @$menu.remove

  select: ->
    @selectcb @.$menu.find('.active').attr('data-value')
    @hide()
    return @

  show: ->
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

  hide: ->
    return @ unless @.shown
    @$menu.hide()
    @shown = false
    $(document).off('mousedown', @.hide)
    return @

  toggle: ->
    if @shown
    then @hide()
    else @show()
    return @

  setItems: @render
  render: (items) ->
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

  inViewport: (el) ->
    m = @$menu
    elOffset = el.offset().top - m.offset().top
    return 0 < elOffset && elOffset < m.height()

  next: (event) ->
    active = @$menu.find('.active').removeClass('active')
    next   = active.next()

    next = $(@$menu.find('li')[0]) unless next.length

    next.addClass('active')
    @inViewport(next) || next[0].scrollIntoView()

  prev: (event) ->
    active = @$menu.find('.active').removeClass('active')
    prev   = active.prev()

    prev = @$menu.find('li').last() unless prev.length

    prev.addClass('active')
    @inViewport(prev) || prev[0].scrollIntoView()

  blur: (e) -> setTimeout((=> @hide()), 150)

  click: (e) ->
    e.stopPropagation()
    e.preventDefault()
    @select()

  mouseenter: (e) ->
    @.$menu.find('.active').removeClass('active')
    $(e.currentTarget).addClass('active')

  mousedown: (e) -> e.stopPropagation(); e.preventDefault()

  draw: (items) ->
    return @.hide() if _.isEmpty items
    return @.render(items).show()
