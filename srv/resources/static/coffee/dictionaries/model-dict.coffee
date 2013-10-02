define ['dictionaries/local-dict',], (ld) ->
  class ModelDict extends ld.dict
    constructor: (@opts) ->
      @model = @opts.dict
      $.bgetJSON "/_/#{@model}", (@items) =>
        @source = ({value: i.id, label: i.label} for i in @items)

    getLab: (val) -> @dictValues()[val]

  dict: ModelDict
