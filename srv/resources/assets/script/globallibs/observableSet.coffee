ko.observableSet = (init) ->
  init = init || {}
  if not _.isObject(init) and not _.isArray(init)
    throw new Error "observableSet: Argument should be array or object,
 got #{init}"
  if _.isArray init
    ibak = init
    init = {}
    init[i] = true for i in ibak

  # have to use this dummy observalble so computed write can notify
  # read about update
  recalcNotifier = ko.observable()

  computed = ko.computed
    read: -> recalcNotifier(); _.keys init
    # val is value we want to add or remove from set
    # when del is true value will be removed
    # val can be single value, array or object
    # when val is object write will be aplied to each k, v pair
    write: (val, del = false) ->
      if _.isArray(val) and del == true
        delete init[v] for v in val when init[v]
      else if _.isArray(val)
        init[v] = true for v in val
      else if _.isObject(val)
        computed(k, v) for k, v of val
      else if del == true
        delete init[val]
      else
        init[val] = true
      recalcNotifier.notifySubscribers()

  computed.truncate = -> init = {}

  return computed