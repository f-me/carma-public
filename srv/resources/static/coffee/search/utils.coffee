define ["model/main", "sync/datamap"], (Main, DataMap) ->

  buildKVMS = (models, raws) -> _.map raws, (r) -> buildKVM models, r

  buildKVM = (models, rs) ->
    r = {}
    for n, m of models
      mapper = new DataMap.Mapper(m)
      r[n] = Main.buildKVM m, { fetched: mapper.s2cObj rs[n] }
    return r

  mkResultObservable: (kvm, models) ->
    robs = ko.observable([])
    ko.computed
      read:      -> robs()
      write: ({values, next, prev}) ->
        if kvm._meta.pager
          kvm._meta.pager.next(next)
          kvm._meta.pager.prev(prev)
        robs(buildKVMS models, values)
