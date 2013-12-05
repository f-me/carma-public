define ["utils"], ->

  class FieldsDynView
    constructor: (@searchKVM, {@labels, @groups}, @defaults) ->
      d = _.difference (_.pluck @defaults, 'name'), (_.keys @groups)
      unless _.isEmpty d
        throw new Error("Unknown groups: #{d}, available: #{_.keys @groups}")

      fixed   = _.chain(@defaults).filter((f) -> f.fixed).pluck('name').value()

      dyndefs = _.chain(@defaults).reject((f) -> f.fixed).pluck('name').value()

      @dynamic    = ko.observableArray dyndefs
      @showFields = ko.computed => fixed.concat @dynamic()
      @hiddenFields = ko.computed =>
        _.difference (_.keys @groups), @showFields()

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
    dynView = new FieldsDynView searchKVM, {labels, groups}, defaults
    labels: labels
    groups: groups
    fields: dynView.showFields
    hidden: dynView.hiddenFields

  transformFields: (searchKVM, models) ->
    labels = {}
    fh     = {}
    groups = {}
    for n,m of models
      fh[n]     = arrToObj 'name', m.fields, (f) -> f
      for f in m.fields
        groups["#{n}_#{f.name}"] ?= {}
        groups["#{n}_#{f.name}"][n] ?= []
        groups["#{n}_#{f.name}"][n].push f
        labels["#{n}_#{f.name}"] = f.meta?.label if f.meta?.label

    for f in searchKVM._meta.model.fields when not f.meta?.nosearch?
      groups[f.name] = {}
      for o in f.meta.search.original
        if "#{o.model}_#{o.name}" != f.name
          delete groups["#{o.model}_#{o.name}"]
        groups[f.name][o.model] ?= []
        groups[f.name][o.model].push fh[o.model][o.name]

      labels[f.name] = f.meta.label if f.meta?.label?
    return { labels: labels, groups: groups }
