# Card numbers dropdown list
define ["lib/meta-dict"], (m) ->
  class CardsDict extends m.dict
    find: (q, cb) ->
      program = global.viewsWare["case-form"].knockVM["program"]()
      # Start searching from 4 digits
      return cb({}) if q.length < 4 or _.isEmpty program
      $.getJSON "/contracts/findByCard/#{program}/#{q}", (r) =>
        @found = r
        a = for i in r
          do (i) ->
            make  = window.global.dictValueCache.CarMakers[i.make]  || i.make
            model = window.global.dictValueCache.CarMakers[i.model] || i.model
            tpl = _.foldl([ "№{{cardNumber}}"
                          , "{{#vin}}<br />{{vin}}{{/vin}}"
                          , "{{#make}}<br />{{make}} {{model}}{{/make}}"
                          , "{{#buyDate}}<br />{{buyDate}}{{/buyDate}}"
                          ], ((m, s) -> m + s), "")
            Mustache.render tpl, i
        # Assume that as input size increases, suggestion list narrows
        # down to a single option which is then selected without extra
        # user interaction
        if r.length == 1
          @id2val 0
        else
          cb a

    # Assume that id2val is only called when a dictionary item is
    # selected by user and load contract into case when this happens
    id2val: (i) ->
      loadContractCard @found[i].cid
      @found[i].cardNumber

  dict: CardsDict
