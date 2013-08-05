define ["lib/meta-dict"], (m) ->
  class LocalDict extends m.dict
    constructor: (@opts) ->
      @dict   = @opts.dict
      @kvm    = @opts.kvm
      @parent = @opts.parent
      @bounded= @opts.bounded
      @s = window.global.dictionaries[@dict] || @_retrieve(@dict)
      unless @s
        throw new Error("Unknown dictionary #{$(@el).attr('data-source')}")
      if @parent and _.isFunction @kvm[@parent]
        @kvm[@parent].subscribe (val) =>
          @source = @s.entries[val]
          @dictValueCache = null
          @dictLabelCache = null
        @kvm[@parent].valueHasMutated()
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

    find: (q, cb) ->
      @q = q
      return cb(@dictValues()) if _.isEmpty(q)
      r = {}
      r[k] = v for k,v of @dictValues() when @match(q, v)
      @found = r
      cb(r)

    match: (q, item) ->
      ~item.toLowerCase().indexOf(q.toLowerCase())

    highlight: (item) ->
      item.replace new RegExp("(#{@q})", 'ig'), ($1, match) ->
        "<strong>#{match}</strong>"

    draw: (items) ->
      r = {}
      r[k] = v for k, v of @found
      r

    _retrieve: (name) ->
      dict = {entries: []}
      $.ajax
        url: "/all/#{name}?fields=id,name&select=isActive==1"
        dataType: "json"
        async: false
        success: (rsp) ->
          dict.entries = for e in rsp
            {value: e.id, label: e.name}
          res = mkCache dict
          window.global.dictLabelCache[name] = res.labelCache
          window.global.dictValueCache[name] = res.valueCache
          window.global.dictionaries[name] = dict
      return dict

  buildGlobalDict = (name, dict) ->
    data = mkCache(dict)
    window.global.dictLabelCache[name] = data.labelCache
    window.global.dictValueCache[name] = data.valueCache
    window.global.dictionaries[name]   = dict

  mkCache = (dict) ->
    labelCache = {}
    valueCache = {}

    if _.isArray(dict.entries)
      for e of dict.entries
        l = dict.entries[e].label
        v = dict.entries[e].value
        labelCache[l] = v
        valueCache[v] = l
    else
      for c of dict.entries
        for e of dict.entries[c]
          l = dict.entries[c][e].label
          v = dict.entries[c][e].value
          if l and v
            labelCache[l] = v
            valueCache[v] = l

    return (
      labelCache: labelCache
      valueCache: valueCache)


  dict: LocalDict
  buildCache: (localDictionaries) ->
    dictLabelCache = {}
    dictValueCache = {}

    # Build caches (TODO: Do this on server some day)
    for d of localDictionaries
      do (d) ->
        res = mkCache localDictionaries[d]
        dictLabelCache[d] = res.labelCache
        dictValueCache[d] = res.valueCache

    return (
      labelCache: dictLabelCache
      valueCache: dictValueCache)
