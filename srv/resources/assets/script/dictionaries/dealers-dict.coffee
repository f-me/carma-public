define ["dictionaries/local-dict", ], (ld) ->
  class DealersDict extends ld.dict
    constructor: (@opts) ->
      @kvm = opts.kvm
      @cacheByVal = {}
      @cacheByLab = {}

      # We use DealersDict on the case screen and on the contracts screen also.
      # This is the reason for the double name check in the following line.
      carMake = (@kvm.car_make || @kvm.carMake)

      updateSource = (v) =>
        return if _.isEmpty v
        $.getJSON "/dealers/#{v}", (@dealers) =>
          @source = ({value: i.id, label: i.name} for i in @dealers)
          @dictValueCache = null
          @dictLabelCache = null

      carMake?.subscribe updateSource
      updateSource(carMake()) if carMake?

    getLab: (val) ->
      if val?
        res = @dictValues()[val]
        unless res
          $.bgetJSON "/_/Partner/#{val}", (rsp) -> res = rsp['name']
        res

  dict: DealersDict
