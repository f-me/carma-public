define ['dictionaries/local-dict',], (ld) ->
  class ModelDict extends ld.dict
    constructor: (@opts) ->
      @model = @opts.dict
      @key   = @opts.meta?.dictionaryKey || "id"
      @label = @opts.meta?.dictionaryLabel || "label"
      # Send numeric ids in strings when storing a dictionary value
      # (old models compatibility)
      @fun =
        if @opts.meta?.dictionaryStringify || false
          String
        else
          _.identity
      $.bgetJSON "/_/#{@model}", (@items) =>
        @source = ({value: @fun(i[@key]), label: i[@label]} for i in @items)

    getLab: (val) -> @dictValues()[val]

  dict: ModelDict
