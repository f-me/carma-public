define [], ->
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

  buildGlobalDict = (name, dict) ->
    data = mkCache(dict)
    window.global.dictLabelCache[name] = data.labelCache
    window.global.dictValueCache[name] = data.valueCache
    window.global.dictionaries[name]   = dict


  get: (name) ->
    # if looks like not very usual name
    if name.match(/:/)
      d = name.match(/(.*):(.*)/)
      [fn, args] = [d[1], d[2].split(',').map (e) -> e.trim()]
      dict = window[fn](args)
      buildGlobalDict(name, dict)
      return dict

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
