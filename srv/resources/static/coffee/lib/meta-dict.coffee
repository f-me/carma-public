define [], ->
  class MetaDict
    constructor: (@opts) ->

    getVal: _.identity
    getLab: _.identity

    lookup: (q, cb) -> cb({})

    id2val: _.identity

  dict: MetaDict
