define ["lib/meta-dict", ], (m) ->
  class DealersDict extends m.dict
    constructor: (@opts) ->
      @kvm = opts.kvm
      @cacheByVal = {}
      @cacheByLab = {}

    find: (q, cb) ->
      # We use DealersDict on the case screen and on the contracts screen also.
      # This is the reasson for the double name check in the following line.
      carMake = @kvm.car_make?() || @kvm.carMake()
      $.getJSON "/dealers/#{carMake}", (@dealers) =>
        for d in @dealers
          @cacheByLab[d.name] = d.id
          @cacheByVal[d.id]   = d.name
        cb(_.pluck @dealers, 'name')

    id2val: (i) -> @dealers[i].id

    getVal: (lab) -> @cacheByLab[lab] || lab

    getLab: (val) ->
      if val?.match /^\d+$/
        res = @cacheByVal[val]
        if not res
          $.ajax
            url: "/_/partner/#{val}"
            dataType: "json"
            async: false
            success: (rsp) -> res = rsp['name']
        res

  dict: DealersDict
