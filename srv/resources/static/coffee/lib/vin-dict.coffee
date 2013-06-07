define ["lib/meta-dict", ], (m) ->
  class VinDict extends m.dict
    lookup: (q, cb) ->
      return cb({}) if q.length < 5
      $.getJSON "/vin/reverseLookup/#{q}", (r) =>
        @found = _.pluck r, 'vin'
        a = for i in r
          do (i) ->
            make  = window.global.dictValueCache.CarMakers[i.make]  || i.make
            model = window.global.dictValueCache.CarMakers[i.model] || i.model
            "#{i.vin} <br /> #{make} #{model}"
        cb(a)

    id2val: (i) -> @found[i]

  dict: VinDict