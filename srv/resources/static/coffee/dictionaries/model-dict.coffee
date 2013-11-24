define ['dictionaries/local-dict',], (ld) ->
  class ModelDict extends ld.dict
    constructor: (@opts) ->
      @model = @opts.dict
      @key   = @opts.meta.dictionaryKey || "id"
      $.bgetJSON "/_/#{@model}", (@items) =>
        @source = ({value: i[@key], label: i.label} for i in @items)

    getLab: (val) -> @dictValues()[val]

  dict: ModelDict
