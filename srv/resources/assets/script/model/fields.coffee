define [], ->

  # add 'begin' and 'end' properties to the observable and put interval
  # object into obs when it is ready
  interval: (obs) ->
    proxy = { begin: null, end: null }
    if !_.isEmpty(obs()) and obs().length == 2
      proxy.begin = obs()[0]
      proxy.end   = obs()[1]
    updateInterval = ->
      if proxy.begin and proxy.end
        obs([proxy.begin, proxy.end])
      else
        obs(null)
    obs.subscribe (v) ->
      proxy.begin = if v then v[0] else null
      proxy.end   = if v then v[1] else null
    obs.begin = ko.computed
      read: -> obs(); proxy.begin
      write: (v) ->
        proxy.begin = v
        updateInterval()

    obs.end = ko.computed
      read: -> obs(); proxy.end
      write: (v) ->
        proxy.end = v
        updateInterval()
    return obs
