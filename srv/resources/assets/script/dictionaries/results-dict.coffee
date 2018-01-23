{_} = require "carma/vendor"
ld = require "carma/dictionaries/local-dict"

# Nested ActionResult dictionary
#
# dictionaryParent meta is required and must point to an ActionType
# field. This dictionary does not auto-update when a parent field is
# changed (action type is normally immutable)
class ResultsDict extends ld.dict
  constructor: (@opts) ->
    @model   = @opts.dict
    @kvm     = @opts.kvm
    @bounded = @opts.bounded

    @parent  = @opts.parent
    unless _.isFunction @kvm[@parent]
      console.error "Parent of results-dict not set"

    @key      = @opts.meta?.dictionaryKey || "id"
    @label    = @opts.meta?.dictionaryLabel || "label"

    @bgetJSON "/_/ActionResult/", (@allItems) =>
      # Fetch full key-label mapping
      @allValuesMap =
        _.reduce @allItems, ((m,i) => m[i[@key]] = i[@label]; m), {}
      # Fetch [(type, result)] mapping
      @bgetJSON "/backoffice/allActionResults/", (@resultPairs) =>
        aType = @kvm[@parent]()
        @updateSource(_.filter @resultPairs, (e) => e[0] == aType)
        @dictValueCache = null
        @dictLabelCache = null

  updateSource: (pairs) ->
    @source = ({value: p[1], label: @allValuesMap[p[1]]} for p in pairs)

  getLab: (val) -> (@allValuesMap || @dictValues())[val]

module.exports =
  dict: ResultsDict
  name: 'ResultsDict'
