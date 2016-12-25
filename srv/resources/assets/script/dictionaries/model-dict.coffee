define ['dictionaries/local-dict',], (ld) ->
  # Dictionaries based on new models.
  #
  # Extra metas supported by ModelDict-dictionaries, in addition to
  # metas of LocalDict:
  #
  # - dictionaryKey
  # - dictionaryLabel
  # - dictionaryParentKey
  class ModelDict extends ld.dict
    constructor: (@opts) ->
      @model = @opts.dict

      @kvm     = @opts.kvm
      @parent  = @opts.parent
      @bounded = @opts.bounded

      @key      = @opts.meta?.dictionaryKey || "id"
      @label    = @opts.meta?.dictionaryLabel || "label"
      parentKey = @opts.meta?.dictionaryParentKey || "parent"
      @filterBy = @opts.meta?.filterBy

      @bgetJSON "/_/#{@model}", (@items) =>
        # we need to gracefully handle cases when parent is not yet
        # initialised but child field should be rendered (see #2027)
        @fullValuesMap   = _.reduce(
          @items,
          ((m,i) => m[i[@key]] = i; m),
          {})
        @allValuesMap   = _.reduce(
          @items,
          ((m,i) =>
            # trim labels because sometimes they have \r\n at the end
            # FIXME: maybe we should filter this in DB
            lab = i[@label]
            m[i[@key]] = if lab?.trim then lab.trim() else lab
            m),
          {})
        if @parent and _.isFunction @kvm[@parent]
          updateChildren = (val) =>
            @updateSource(_.filter @items, (e) => e[parentKey] == val)
            @dictValueCache = null
            @dictLabelCache = null
          @kvm[@parent].subscribe updateChildren
          updateChildren(@kvm[@parent]())
        else
          @updateSource @items

    updateSource: (items) ->
      @source = ({value: i[@key], label: i[@label], _e: i} for i in items)
      @allElementsMap = {}
      for i in items
        @allElementsMap[i[@key]] = i

    getLab: (val) -> @allValuesMap[val]

    # Fetch full element data by dictionary key
    getElement: (val) -> @allElementsMap[val]

    sorter: (a, b) =>
      if @fullValuesMap
        @fullValuesMap[a.key].ord - @fullValuesMap[b.key].ord

    dictValues: ->
      if !@dictValueCache
        src =
          if @filterBy
          then _.filter @source, (o) => o._e[@filterBy]
          else @source
        @dictValueCache =
          _.reduce src, ((m, i) -> m[i.value] = i.label; m), {}
      @dictValueCache

  dict: ModelDict
  name: 'ModelDict'
