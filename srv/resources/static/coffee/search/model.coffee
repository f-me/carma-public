define ["utils"], ->

  class FieldsDynView
    constructor: (@searchKVM, {@labels, @groups}, @defaults) ->
      d = _.difference (_.pluck @defaults, 'name'), (_.keys @groups)
      # console.log _.keys @groups
      unless _.isEmpty d
        throw new Error("Unknown groups: #{d}, available: #{_.keys @groups}")

      fixed   = _.chain(@defaults).filter((f) -> f.fixed).pluck('name').value()

      dyndefs = _.chain(@defaults).reject((f) -> f.fixed).pluck('name').value()

      @dynamic    = ko.observableArray dyndefs
      @showFields = ko.computed => fixed.concat @dynamic()
      @sfieldsh   = arrToObj 'name', @searchKVM._meta.model.fields

      for f in @searchKVM._meta.model.fields when not f.meta?.nosearch?
        do (f) =>
          @searchKVM[f.name].subscribe (v) =>
            @updateFields(f.name) if (v)

    updateFields: (f) ->
      switch @sfieldsh[f].meta.search.matchType
        when "MatchExact"    then @removeField f
        when "MatchFuzzy"    then @addField f
        when "MatchArray"    then @addField f
        when "MatchInterval" then @addField f

    removeField: (f) ->
      @showFields.remove (v) -> _.isEqual f, v
      @addFree()

    addField:    (f) ->
      return if _.contains @dynamic(), f
      @dynamic.shift()
      @dynamic.push(f)

    addFree: ->
      f = @showfields()
      t = _.keys @groups
      d = _.difference f, t
      @dynamic.pop d[0]

  mkFieldsDynView: (searchKVM, {labels, groups}, defaults) ->
    # console.log (new FieldsDynView searchKVM, {labels, groups}, defaults).showFields()
    labels: labels
    groups: groups
    fields: (new FieldsDynView searchKVM, {labels, groups}, defaults).showFields

  transformFields: (searchKVM, models) ->
    labels = {}
    fh     = {}
    groups = {}
    for n,m of models
      labels[n] = arrToObj 'name', m.fields, (f) -> f.meta.label
      fh[n]     = arrToObj 'name', m.fields, (f) -> f
      for f in m.fields
        groups["#{n}_#{f.name}"] ?= {}
        groups["#{n}_#{f.name}"][n] ?= []
        groups["#{n}_#{f.name}"][n].push f

    for f in searchKVM._meta.model.fields when not f.meta?.nosearch?
      groups[f.name] = {}
      for o in f.meta.search.original
        if "#{o.model}_#{o.name}" != f.name
          delete groups["#{o.model}_#{o.name}"]
        groups[f.name][o.model] ?= []
        groups[f.name][o.model].push fh[o.model][o.name]

      labels[f.name] = f.meta.label if f.meta?.label? and not labels[f.name]
    return { labels: labels, groups: groups }
