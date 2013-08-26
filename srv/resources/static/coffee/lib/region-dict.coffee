define ["lib/meta-dict", ], (m) ->
  class RegionDict extends m.dict
    constructor: () ->
      @dict   = 'region'
      regions = {entries: []}
      $.bgetJSON "/all/#{@dict}", (rsp) =>
        _.each rsp, (region) ->
          region.cities = region.cities?.split(',')

        regions.entries = rsp
        window.global.dictionaries[@dict] = regions
      @source = regions.entries

    findRegionByCity: (city) ->
      _.filter @source, (region) ->
        _.include region.cities, city

  dict: RegionDict
