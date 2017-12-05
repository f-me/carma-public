{$, _, ko} = require "carma/vendor"

typeaheadDefaults =
  source: []
  items: 8
  menu: '<ul class="typeahead dropdown-menu"></ul>'
  item: '<li><a href="#"></a></li>'
  minLength: 1
  scrollHeight: 0
  autoSelect: true


class ThMenu
  constructor: (options) ->
    @options     = $.extend {}, typeaheadDefaults, options
    @matcher     = @options.matcher     || @matcher
    @sorter      = @options.sorter      || @sorter
    @highlighter = @options.highlighter || @highlighter
    @$menu       = $(@options.menu).appendTo 'body'

    @selectcb    = options.select
    @dict        = options.dict

    @shown = false

    @$menu
      .on 'click.typeahead',            $.proxy @click, @
      .on 'mousedown.typeahead',        $.proxy @mousedown, @
      .on 'mouseenter.typeahead', 'li', $.proxy @mouseenter, @

  setElement: (el) =>
    @$element = $(el)
    @$element
      .on 'blur.typeahead',     $.proxy @blur, @
      .on 'keypress.typeahead', $.proxy @keypress, @
      .on 'keyup.typeahead',    $.proxy @keyup, @

    # if $.browser.webkit or $.browser.msie
    # FIXME: test this on ff, in case of failure find way to detect browser
    # $.browser is deprecated
    @$element.on 'keydown.typeahead', $.proxy @keypress, @

  destructor: =>
    if @$element
      @$element
        .off 'blur.typeahead'
        .off 'keypress.typeahead'
        .off 'keyup.typeahead'

      if ($.browser.webkit or $.browser.msie) and @$element
        @$element.off 'keydown.typeahead'

    do @$menu.remove
    @$menu    = null
    @$element = null


  select: =>
    # TODO FIXME types should be provided by meta information (am i right?)
    f = switch typeof @dict.source?[0]?.value
      when "number" then parseInt
      when "string" then String
      else _.identity

    @selectcb f @$menu.find('.active').attr 'data-value'
    do @hide
    do @$element.change
    this

  show: =>
    windowWidth = $(window).width()

    pos = $.extend {}, @$element.offset(),
      height: @$element[0].offsetHeight
      width: @$element[0].offsetWidth

    pos.right = windowWidth - pos.left - pos.width

    [left, right] = if pos.right > windowWidth * 0.6 \
                       then [pos.left, 'auto']
                       else ['auto', pos.right]

    # if element in bottom area of the screen
    if pos.top > $(window).height() / 2
      # .. show menu above it
      paddingBottom = pos.height / 2 + 4
      @$menu.css
        top: pos.top - @$menu.height() - paddingBottom
        left: left
        right: right
        minWidth: "#{@$element.width()}px"
        maxWidth: '55%'
    else
      # .. show menu below it
      @$menu.css
        top: pos.top + pos.height
        left: left
        right: right
        minWidth: "#{@$element.width()}px"
        maxWidth: '55%'

    do @$menu.show
    @shown = true
    $(document).on 'mousedown', $.proxy @hide, @
    return @

  hide: =>
    return this unless @shown
    do @$menu.hide
    @shown = false
    $(document).off 'mousedown', @hide
    this

  toggle: =>
    if @shown \
       then do @hide
       else do @show
    this

  render: (items) =>
    itms = $.map items, (v, k) -> key: k, label: v
    itms = itms.sort @dict.sorter if @dict.sorter
    itms = itms.map ({key, label}) =>
      i = if _.isFunction @options.item
            @options.item.call this, item
          else
            el = $(@options.item)
            el.find('a').html(label)
            el
      i.attr 'data-value', key
      i[0]

    $(itms).first().addClass 'active'
    @$menu.html itms
    this

  inViewport: (el) =>
    m = @$menu
    elOffset = el.offset().top - m.offset().top
    0 < elOffset && elOffset < m.height()

  next: (event) =>
    active = @$menu.find('.active').removeClass 'active'
    next   = active.next()
    next   = $(@$menu.find('li')[0]) unless next.length

    next.addClass 'active'
    @inViewport(next) || next[0].scrollIntoView(false)

  prev: (event) =>
    active = @$menu.find('.active').removeClass('active')
    prev   = active.prev()

    prev = @$menu.find('li').last() unless prev.length

    prev.addClass 'active'
    @inViewport(prev) || prev[0].scrollIntoView(false)

  blur: (e) => setTimeout (=> do @hide), 150

  click: (e) =>
    do e.stopPropagation
    do e.preventDefault
    do @select

  mouseenter: (e) =>
    @$menu.find('.active').removeClass 'active'
    $(e.currentTarget).addClass 'active'

  mousedown: (e) ->
    do e.stopPropagation
    do e.preventDefault

  # debounce here will decrease request rate for remote dictionaries
  # still it's small enough to not be noticable for regular dicts
  draw: _.debounce (-> do @_draw), 150

  _draw:  =>
    return this unless @$element
    @dict.lookup @$element.val(), (v) =>
      return do @hide if _.isEmpty v
      return do @render(v).show

  # this methos is for .add-on arrow on the field
  drawAll: =>
    return this unless @$element
    unless @$element.is ':disabled'
      @dict.lookup "", (v) =>
        return do @hide if _.isEmpty v
        return do @render(v).show

  # this methos is for .add-on search on the field
  drawAllForce: =>
    return this unless @$element
    unless @$element.is ':disabled'
      @dict.lookup @$element.val(), (v) =>
        return do @hide if _.isEmpty v
        return do @render(v).show
      , {force: true}


  keyup: (e) =>
    switch e.keyCode
      when 40, 38 # down arrow, up arrow
        do @draw unless @shown
      when 9, 16, 37, 39  # tab, shift, left, right
        return
      when 13     # enter
        do @select if @shown
      when 27     # escape
        do @hide

      else
        do @draw

    do e.stopPropagation
    do e.preventDefault

  keypress: (e) =>
    return unless @shown

    switch e.keyCode
      when 9, 13, 27 # tab, enter, escape
        do e.preventDefault

      when 38        # up arrow
        do e.preventDefault
        do @prev

      when 40        # down arrow
        do e.preventDefault
        do @next

    do e.stopPropagation


module.exports = {ThMenu}
