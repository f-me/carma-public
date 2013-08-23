define ["lib/meta-dict", ], (m) ->
  class RegionDict extends m.dict
    constructor: (@opts) ->
      @dict   = @opts.dict
      @s = window.global.dictionaries[@dict] || @_retrieve(@dict)
      unless @s
        throw new Error("Unknown dictionary #{$(@el).attr('data-source')}")
      @source = @s.entries

    _retrieve: (name) ->
      dict = {entries: []}
      $.bgetJSON "/all/#{name}", (rsp) =>
        _.each rsp, (region) ->
          region.cities = region.cities?.split(',')

        dict.entries = rsp
        window.global.dictionaries[name] = dict
      dict

    findRegionByCity: (city) ->
      _.filter @source, (region) ->
        _.include region.cities, city

  dict: RegionDict
