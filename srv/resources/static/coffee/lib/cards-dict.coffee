# Card numbers dropdown list
define ["lib/meta-dict"], (m) ->
  class CardsDict extends m.dict
    find: (q, cb) ->
      program = global.viewsWare["case-form"].knockVM["program"]()
      return cb({}) if q.length < 4 or _.isEmpty program
      $.getJSON "/contracts/findByCard/#{program}/#{q}", (r) =>
        @found = _.pluck r, 'cardNumber'
        a = for i in r
          do (i) ->
            make  = window.global.dictValueCache.CarMakers[i.make]  || i.make
            model = window.global.dictValueCache.CarMakers[i.model] || i.model
            "#{i.cardNumber}<br/>#{i.vin}<br/>#{make} #{model}<br/>#{i.buyDate}"
        cb(a)

    id2val: (i) -> @found[i]

  dict: CardsDict
