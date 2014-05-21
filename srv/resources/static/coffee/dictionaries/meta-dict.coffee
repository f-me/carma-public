define ["lib/ajax"], (Ajax)->
  class MetaDict extends Ajax
    constructor: (@opts) ->

    getVal: _.identity
    getLab: _.identity

    lookup: (q, cb, opt) -> if @disabled then cb({}) else @find(q, cb, opt)

    # find is what should be redefined in descedant classes
    # lookup is still part of public api, to retrieve data from dict
    find  : (q, cb, opt) -> cb({})

    id2val: _.identity

  dict: MetaDict
