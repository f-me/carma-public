/* =============================================================
 * bootstrap-typeahead.js v2.0.2
 * http://twitter.github.com/bootstrap/javascript.html#typeahead
 * =============================================================
 * Copyright 2012 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ============================================================ */

!function( $ ){

  "use strict"

  var Typeahead = function ( element, options ) {
    this.$element = $(element)
    this.options = $.extend({}, $.fn.typeahead.defaults, options)
    this.matcher = this.options.matcher || this.matcher
    this.sorter = this.options.sorter || this.sorter
    this.highlighter = this.options.highlighter || this.highlighter
    this.$menu = $(this.options.menu).appendTo('body')
    this.options.source = eval(this.options.source)

    if (!this.options.parent)
        this.options.source = this.options.source.entries
    // If data-parent is present, plug only values for this parent value
    else {
        this.options.source = this.options.source.entries[this.options.parent]
        // If no parent values matched, no typeahead items are presented
        if (!this.options.source)
            this.options.source = []
        this.parent = this.options.parent
    }
        
    if (this.options.source.length > 0 && typeof this.options.source[0] == 'object')
        this.source = this.labelsFor(this.options.source)
    else
        this.source = this.options.source

    this.shown = false
    this.listen()
  }

  Typeahead.prototype = {

    constructor: Typeahead

  , select: function () {
      var val = this.$menu.find('.active').attr('data-value')
      this.$element.val(val)
      this.$element.change();
      return this.hide()
    }

  , show: function () {
      var pos = $.extend({}, this.$element.offset(), {
        height: this.$element[0].offsetHeight
      })

      this.$menu.css({
        top: pos.top + pos.height
      , left: pos.left
      , minWidth: this.$element.width() + "px"
      })

      this.$menu.show()
      this.shown = true
      return this
    }

  , hide: function () {
      this.$menu.hide()
      this.shown = false
      return this
    }

  , toggle: function () {
      if (this.shown) this.hide()
      else this.lookup()
    }

  , lookup: function (event) {
      var that = this
        , items
        , q

      this.query = this.$element.val()

      items = $.grep(this.source, function (item) {
        if (!that.query || that.matcher(item)) return item
      })

      items = this.sorter(items)

      if (!items.length) {
        return this.shown ? this.hide() : this
      }

      return this.render(items.slice(0, this.options.items)).show()
    }

  , matcher: function (item) {
      return ~item.toLowerCase().indexOf(this.query.toLowerCase())
    }

  , sorter: function (items) {
      var beginswith = []
        , caseSensitive = []
        , caseInsensitive = []
        , item

      while (item = items.shift()) {
        if (!item.toLowerCase().indexOf(this.query.toLowerCase())) beginswith.push(item)
        else if (~item.indexOf(this.query)) caseSensitive.push(item)
        else caseInsensitive.push(item)
      }

      return beginswith.concat(caseSensitive, caseInsensitive)
    }

  , highlighter: function (item) {
      return item.replace(new RegExp('(' + this.query + ')', 'ig'), function ($1, match) {
        return '<strong>' + match + '</strong>'
      })
    }

  , render: function (items) {
      var that = this

      items = $(items).map(function (i, item) {
        i = $(that.options.item).attr('data-value', item)
        i.find('a').html(that.highlighter(item))
        return i[0]
      })

      items.first().addClass('active')
      this.$menu.html(items)
      return this
    }

  , inViewport: function (el) {
      var m = this.$menu
      var elOffset = el.offset().top - m.offset().top
      return 0 < elOffset && elOffset < m.height()
    }

  , next: function (event) {
      var active = this.$menu.find('.active').removeClass('active')
        , next = active.next()

      if (!next.length) {
        next = $(this.$menu.find('li')[0])
      }

      next.addClass('active')
      this.inViewport(next) || next[0].scrollIntoView()
    }

  , prev: function (event) {
      var active = this.$menu.find('.active').removeClass('active')
        , prev = active.prev()

      if (!prev.length) {
        prev = this.$menu.find('li').last()
      }

      prev.addClass('active')
      this.inViewport(prev) || prev[0].scrollIntoView()
    }

  , listen: function () {
      this.$element
        .on('blur.typeahead',     $.proxy(this.blur, this))
        .on('keypress.typeahead', $.proxy(this.keypress, this))
        .on('keyup.typeahead',    $.proxy(this.keyup, this))

      if ($.browser.webkit || $.browser.msie) {
        this.$element.on('keydown.typeahead', $.proxy(this.keypress, this))
      }

      this.$menu
        .on('click.typeahead', $.proxy(this.click, this))
        .on('mouseenter.typeahead', 'li', $.proxy(this.mouseenter, this))
    }
   
  // Unattach old listeners
  , unlisten: function () {
      this.$element
        .off('blur.typeahead')
        .off('keypress.typeahead')
        .off('keyup.typeahead')

      if ($.browser.webkit || $.browser.msie) {
        this.$element.off('keydown.typeahead')
      }

      this.$menu
        .off('click.typeahead')
        .off('mouseenter.typeahead')
    }

  , keyup: function (e) {
      switch(e.keyCode) {
        case 40: // down arrow
        case 38: // up arrow
          if (!this.shown) this.lookup()
          break

        case 9: // tab
        case 13: // enter
          if (!this.shown) this.lookup()
          else this.select()
          break

        case 27: // escape
          if (!this.shown) return
          this.hide()
          break

        default:
          this.lookup()
      }

      e.stopPropagation()
      e.preventDefault()
  }

  , keypress: function (e) {
      if (!this.shown) return

      switch(e.keyCode) {
        case 9: // tab
        case 13: // enter
        case 27: // escape
          e.preventDefault()
          break

        case 38: // up arrow
          e.preventDefault()
          this.prev()
          break

        case 40: // down arrow
          e.preventDefault()
          this.next()
          break
      }

      e.stopPropagation()
    }

  , blur: function (e) {
      var that = this
      setTimeout(function () { that.hide() }, 150)
    }

  , click: function (e) {
      e.stopPropagation()
      e.preventDefault()
      this.select()
    }

  , mouseenter: function (e) {
      this.$menu.find('.active').removeClass('active')
      $(e.currentTarget).addClass('active')
    }


  , labelsFor: function(items) {
      return $.makeArray($(items).map(function (i, item) {
          return item.label
      }))
    }
  }


  /* TYPEAHEAD PLUGIN DEFINITION
   * =========================== */

  $.fn.typeahead = function ( option ) {
    return this.each(function () {
      var $this = $(this)
        , data = $this.data('typeahead')
        , options = typeof option == 'object' && option
      if (!data) $this.data('typeahead', (data = new Typeahead(this, options)))
      if (typeof option == 'string') data[option]()
    })
  }

  $.fn.typeahead.defaults = {
    source: []
  , items: 100
  , menu: '<ul class="typeahead dropdown-menu"></ul>'
  , item: '<li><a href="#"></a></li>'
  }

  $.fn.typeahead.Constructor = Typeahead


 /* TYPEAHEAD DATA-API
  * ================== */
  function initDictionary (e) {
      var $this = $(this)
      $this.data('parent', $this.attr('data-parent'))
      var typeahead = $this.data('typeahead')
      if (typeahead) {
          // Note the attr since jQuery .data() caches values once
          if ($this.data('parent')) {
              if ($this.data('parent') == typeahead.parent)
                  return typeahead
              else {
                  // If data-parent value has changed, recreate Typehead from scratch
                  typeahead.unlisten()
                  $this.data('typeahead', null);
              }
          }
          else
              return typeahead
      }
      e.preventDefault()
      $this.typeahead($this.data())
      return $this.data('typeahead')
  }

  $(function () {
    $('body').on('focus.typeahead.data-api', '[data-provide="typeahead"]', initDictionary)
    $('body').on('click.typeahead.data-api', '[data-provide="typeahead-toggle"]', function (e) {
        var $this = $(this)
        var inp = $this.parent().prev() // FIXME: less ad-hoc search required
        var dat = inp.data('typeahead') || initDictionary.call(inp, e)
        dat && dat.toggle()
        e.preventDefault()
    })
  })

}( window.jQuery );
