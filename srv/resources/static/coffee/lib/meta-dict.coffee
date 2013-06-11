define [], ->
  class MetaDict
    constructor: (@opts) ->

    getVal: _.identity
    getLab: _.identity

    lookup: (q, cb) -> @find(q, cb) unless @disabled

    # find is what should be redefined in descedant classes
    # lookup is still part of public api, to retrieve data from dict
    find  : (q, cb) -> cb({})

    id2val: _.identity

  dict: MetaDict
