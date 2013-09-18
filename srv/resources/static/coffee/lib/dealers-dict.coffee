define ["lib/local-dict", ], (m) ->
  class DealersDict extends m.dict
    constructor: (@opts) ->
      @kvm = opts.kvm
      @cacheByVal = {}
      @cacheByLab = {}

      # We use DealersDict on the case screen and on the contracts screen also.
      # This is the reason for the double name check in the following line.
      carMake = (@kvm.car_make || @kvm.carMake)?()
      (@kvm.car_make || @kvm.carMake)?.subscribe (v) =>
        $.getJSON "/dealers/#{v}", (@dealers) =>
          @source = ({value: i.id, label: i.name} for i in @dealers)

    getLab: (val) ->
      if val?.match /^\d+$/
        res = @dictValues()[val]
        unless res
          $.bgetJSON "/_/partner/#{val}", (rsp) -> res = rsp['name']
        res

  dict: DealersDict
