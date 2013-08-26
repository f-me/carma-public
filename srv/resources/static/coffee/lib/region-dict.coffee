define ["lib/local-dict", ], (m) ->
  class RegionDict extends m.dict
    constructor: () ->
      @dict   = 'region'
      $.bgetJSON "/all/#{@dict}", (rsp) =>
        _.each rsp, (region) ->
          region.value = region.cities
          delete region.cities

        @source = rsp
        window.global.dictLabelCache[@dict] = @dictLabels()
        window.global.dictValueCache[@dict] = @dictValues()
        window.global.dictionaries[@dict] = {entries: @source}

    dictValues: ->
      iterator = (memo, region) ->
        cities = region.value?.split(',')
        _.each cities, (city) ->
          memo[city] = if memo[city]
            [memo[city], region.label].join(', ')
          else
            region.label
        memo

      @dictValueCache ||=
        _.reduce @source, iterator, {}

  dict: RegionDict
