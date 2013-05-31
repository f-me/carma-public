class LocalDict
  constructor: (@el, @acc, @allBindigns, @kvm) ->
    @s = window.global.dictionaries[$(@el).attr('data-source')]
    unless @s
      throw new Error("Unknown dictionary #{$(@el).attr('data-source')}")
    p = @allBindigns().attr?['data-parent']
    if _.isFunction p
      p.subscribe (val) => @source = @s.entries[val]
    else
      @source = @s.entries

  get: ->
    _.reduce @source, ((m, i) -> m[i.value] = i.label; m), {}

  lookup: (q) ->
    @q = q
    return @get() if _.isEmpty(q)
    r = {}
    r[k] = v for k,v of @get() when @match(q, v)
    @found = r
    r

  match: (q, item) ->
    ~item.toLowerCase().indexOf(q.toLowerCase())

  highlight: (item) ->
    item.replace new RegExp("(#{@q})", 'ig'), ($1, match) ->
      "<strong>#{match}</strong>"

  draw: (items) ->
    r = {}
    r[k] = v for k, v of @found
    r
