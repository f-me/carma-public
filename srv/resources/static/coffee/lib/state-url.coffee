define ["utils"], (Utils) ->

  serializeKvm = (k) ->
    o = {}
    for f in k._meta.model.fields when k[f.name]()
      o[f.name] = k[f.name]()
    JSON.stringify(o)

  serialize = (v) ->
    switch
      when v._meta.model?
        serializeKvm v
      else
        JSON.stringify(v)

  save: (kv) ->
    o = {}
    for k, v of kv
      o[k] = serialize(v)
    Utils.setUrlParams o

  load: (kv) ->
    nkv = $.extend true, {}, kv
    prms = Utils.getUrlParams()
    for k, v of prms
      try
        prms[k] = JSON.parse v
      catch e
        console.error "some bad data get into state loader:", v, "for key:", k,
          "I will delete this, hope you don't mind"
        delete prms[k]
    for k, v of kv when prms[k]
      if v._meta.model
        for f in v._meta.model.fields when prms[k][f.name]
          v[f.name] prms[k][f.name]
      else
        nkv[k] = prms[k]
    return nkv

  statify: (kvm, state) ->
    state ?= {}
    self = this
    for f in kvm._meta.model.fields
      do (f) ->
        state[kvm._meta.model.name] = kvm
        kvm[f.name].subscribe -> self.save state
    load = {}
    load[kvm._meta.model.name] = kvm
    load

  statifyMany: (kvms, state) ->
    state ?= {}
    loaders = (statify k, state for k in kvms)
    _.foldl loaders, ((a, c) -> _.extend a, c), {}