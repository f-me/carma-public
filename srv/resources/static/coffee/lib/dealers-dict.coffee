define ["lib/meta-dict", ], (m) ->
  class DealersDict extends m.dict
    constructor: (@opts) ->
      @kvm = opts.kvm
      @valueCache = {}
      $.ajax
        url: "/dealers"
        dataType: "json"
        async: false
        success: (rsp) =>
          for e in rsp
            @valueCache[e.id] = e.name

    find: (q, cb) ->
      carMake = @kvm.car_make()
      $.getJSON "/dealers/#{carMake}", (@dealers) =>
        cb(_.pluck @dealers, 'name')

    id2val: (i) -> @dealers[i].id

    getLab: (val) -> @valueCache[val] || val

  dict: DealersDict
