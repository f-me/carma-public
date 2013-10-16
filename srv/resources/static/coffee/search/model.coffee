define [], ->

  class FieldsDynView
    constructor: (@searchKVM, {@labels, @groups}, @defaults) ->
      fixed   = _.chain(@defaults)
        .filter((f) -> f.fixed)
        .map((f) -> "#{f.model}_#{f.name}")
        .value()

      dyndefs = _.chain(@defaults)
        .reject((f) -> f.fixed)
        .map((f) -> "#{f.model}_#{f.name}")
        .value()

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
    labels: labels
    groups: groups
    fields: (new FieldsDynView searchKVM, {labels, groups}, defaults).showFields

  transformFields: (searchKVM, models) ->
    convn = (m) -> (f) -> "#{m.name}_#{f.name}"
    labels = {}
    fh     = {}
    for m in models
      $.extend labels, (arrToObj (convn m), m.fields, (f) -> f.meta.label)
      $.extend fh,     (arrToObj (convn m), m.fields, (f) -> [f])

    groups  = $.extend true, {}, fh
    for f in searchKVM._meta.model.fields
      groups[f.name] = _.map f.meta.search.original, (n) ->
        delete groups["#{n.model}_#{n.name}"]
        fh["#{n.model}_#{n.name}"]

      labels[f.name] = f.meta.label if f.meta?.label? and not labels[f.name]
    return { labels: labels, groups: groups }
