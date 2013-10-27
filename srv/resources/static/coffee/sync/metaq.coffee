define ["sync/datamap"], (m) ->
  class MetaQueue

    constructor: (@kvm, @model) ->
      @ftypes = {}
      @ftypes[f.name] = f.type for f in @model.fields

    toRawObj: ->
      r = {}
      r[f.name] = @kvm[f.name]() for f in @model.fields when @kvm[f.name]()
      r['id']   = @kvm.id()
      m.c2sObj r, @ftypes

    toJSON: -> JSON.stringify @toRawObj()
