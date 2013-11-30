define ["model/main", "sync/datamap"], (Main, DataMap) ->

  # We receiving all fieldnames in lowercase (at least for now)
  # so we have to translate them into normal ones according to
  # their model
  fixNames = (ssmodels, v) -> _.map v, (v) -> fixName ssmodels, v

  fixName = (models, rawInst) ->
    fixed = {}
    for m, fs of models
      fnames = _.pluck fs.fields, 'name'
      fields = {}
      for fn in fnames
        fields[fn] = rawInst[m.toLowerCase()][fn]
      fixed[m] = fields
    return fixed

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
        robs(buildKVMS models, fixNames models, values)
