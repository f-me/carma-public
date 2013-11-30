define ["utils"], (Utils) ->

  persistKVM: (name, obj) ->
    _save = _.debounce((=> @save(obj)), 500)
    kvm = obj[name]
    for f in kvm._meta.model.fields
      kvm[f.name].subscribe => _save(obj)


  save: (obj) -> Utils.setUrlParams { state: JSON.stringify obj }

  load: (kv) ->
    state = Utils.getUrlParams().state
    return unless state
    try
      stateobj = JSON.parse state
    catch e
      console.error "can't parse obj", state
    for k, v of stateobj
      if kv[k]
        kv[k].fromJSON v
      else
        console.warn "Can't restore state for #{k}, no such source object"
    return kv
