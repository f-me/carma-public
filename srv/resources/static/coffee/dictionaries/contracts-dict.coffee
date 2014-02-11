define ["dictionaries/meta-dict", ], (m) ->
  class ContractsDict extends m.dict
    find: (q, cb) ->
      return cb({}) if q.length < 4
      $.getJSON "/searchContracts/?query=#{q}", (r) =>
        @found = _.pluck r, 'vin'
        a = for i in r
          do (i) ->
            make  = window.global.dictValueCache.CarMakers[i.make]  || i.make
            model = window.global.dictValueCache.CarMakers[i.model] || i.model
            "#{i.vin} <br/> #{make} #{model} <br/> #{i.buyDate}"
        cb(a)

    id2val: (i) -> @found[i]

  dict: ContractsDict
