define ['lib/local-dict',], (m) ->
  class ModelDict extends m.dict
    constructor: (@opts) ->
      @model = @opts.dict
      $.bgetJSON "/_/#{@model}", (@items) =>
        @source = ({value: i.id, label: i.label} for i in @items)

    getLab: (val) -> @dictValues()[val]

  dict: ModelDict
