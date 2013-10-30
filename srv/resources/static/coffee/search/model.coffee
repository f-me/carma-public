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

  fromname = (s) ->
    model: s.split('_')[0]
    name:  s.split('_')[1..-1].join('_')

  mkFieldsDynView: (searchKVM, {labels, groups}, defaults) ->
    # console.log (new FieldsDynView searchKVM, {labels, groups}, defaults).showFields()
    labels: labels
    groups: groups
    fields: (new FieldsDynView searchKVM, {labels, groups}, defaults).showFields

  transformFields: (searchKVM, models) ->
    convn = (m) -> (f) -> "#{m.name}_#{f.name}"
    labels = {}
    fh     = {}
    for n,m of models
      $.extend labels, (arrToObj (convn m), m.fields, (f) -> f.meta.label)
      $.extend fh,     (arrToObj (convn m), m.fields, (f) -> [f])

    groups  = $.extend true, {}, fh
    for f in searchKVM._meta.model.fields when not f.meta?.nosearch?
      groups[f.name] = _.map f.meta.search.original, (n) ->
        delete groups["#{n.model}_#{n.name}"]
        fh["#{n.model}_#{n.name}"]

      labels[f.name] = f.meta.label if f.meta?.label? and not labels[f.name]
    return { labels: labels, groups: groups }
