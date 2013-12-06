define ['dictionaries/local-dict',], (ld) ->
  # Dictionaries based on new models.
  #
  # Extra metas supported by ModelDict-dictionaries, in addition to
  # metas of LocalDict:
  #
  # - dictionaryKey
  # - dictionaryLabel
  # - dictionaryParentKey
  # 
  # - dictionaryStringify (true when ModelDict is used in a field of
  #   an old-style model)
  class ModelDict extends ld.dict
    constructor: (@opts) ->
      @model = @opts.dict

      @kvm     = @opts.kvm
      @parent  = @opts.parent
      @bounded = @opts.bounded

      @key      = @opts.meta?.dictionaryKey || "id"
      @label    = @opts.meta?.dictionaryLabel || "label"
      parentKey = @opts.meta?.dictionaryParentKey || "parent"
      
      # Send numeric ids in strings when storing a dictionary value
      # (old models compatibility)
      @fun =
        if @opts.meta?.dictionaryStringify || false
          String
        else
          _.identity

      $.bgetJSON "/_/#{@model}", (@items) =>
        if @parent and _.isFunction @kvm[@parent]
          @kvm[@parent].subscribe (val) =>
            fun = @fun
            @updateSource(_.filter @items, (e) -> fun(e[parentKey]) == val)
            @dictValueCache = null
            @dictLabelCache = null
          @kvm[@parent].valueHasMutated()
        else
          @updateSource @items

    updateSource: (items) ->
      @source = ({value: @fun(i[@key]), label: i[@label]} for i in items)

    getLab: (val) -> @dictValues()[val]

  dict: ModelDict
