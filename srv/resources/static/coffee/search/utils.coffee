define ["model/main"], (Main) ->

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
        fields[fn] = rawInst[m.toLowerCase()][fn.toLowerCase()]
      fixed[m] = fields
    return fixed

  buildKVMS = (models, raws) -> _.map raws, (r) -> buildKVM models, r

  buildKVM = (models, rs) ->
    r = {}
    for n, m of models
      r[n] = Main.buildKVM m, { fetched: rs[n] }
    return r

  mkResultObservable: (models) ->
    robs = ko.observable([])
    ko.computed
      read:      -> robs()
      write: (v) -> robs(buildKVMS models, fixNames models, v)
