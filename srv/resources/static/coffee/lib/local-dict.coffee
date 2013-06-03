define ["dictionaries"], (dict) ->
  class LocalDict
    constructor: (@opts) ->
      @dict   = @opts.dict
      @kvm    = @opts.kvm
      @parent = @opts.parent
      @bounded= @opts.bounded
      @s = window.global.dictionaries[@dict] || dict.get(@dict)
      unless @s
        throw new Error("Unknown dictionary #{$(@el).attr('data-source')}")
      if @parent and _.isFunction @kvm[@parent]
        @kvm[@parent].subscribe (val) =>
          @source = @s.entries[val]
          @dictValueCache = null
          @dictLabelCache = null
      else
        @source = @s.entries

    dictValues: ->
      @dictValueCache ||=
        _.reduce @source, ((m, i) -> m[i.value] = i.label; m), {}

    dictLabels: ->
      @dictLabelCache ||=
        _.reduce @source, ((m, i) -> m[i.label] = i.value; m), {}

    getVal: (lab) ->
      @dictLabels()[lab]

    getLab: (val) ->
      # FIXME: have to use global cache, because
      # @dictValues will be recalculated in case of
      # parent value is changed
      window.global.dictValueCache[@dict][val]
      # @dictValues()[val]

    lookup: (q) ->
      @q = q
      return @dictValues() if _.isEmpty(q)
      r = {}
      r[k] = v for k,v of @dictValues() when @match(q, v)
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

  LocalDict: LocalDict