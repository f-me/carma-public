{$, _, ko} = require "carma/vendor"
require "carma/utils"

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
    # storage for shifted user fields
    # free fields will be out of here
    @history = []

    for f in @searchKVM._meta.model.fields when not f.meta?.nosearch?
      do (f) =>
        @searchKVM[f.name].subscribe (v) =>
          if (v)
            @updateFields(f.name)
          else
            unless @isDefault f.name
              @removeField f.name
          # rewind to the first page of search results
          @searchKVM._meta.pager.offset 0

  isDefault: (f) ->
    _.contains _.pluck(@defaults, 'name'), f

  updateFields: (f) ->
    switch @sfieldsh[f].meta.search.matchType
      when "MatchExact"    then @removeField f
      when "MatchFuzzy"    then @addField f
      when "MatchArray"    then @addField f
      when "MatchInterval" then @addField f

  removeField: (f) =>
    unless _.isEmpty(@dynamic.remove (v) -> _.isEqual f, v)
      @addFree()

  addField:    (f) =>
    return if _.contains @showFields(), f
    @changeHistory @dynamic.shift()
    @dynamic.push(f)

  addFree: =>
    unless _.isEmpty @history
      # add a recently shifted field
      @dynamic.push @history.shift()
    else
      # add a random field
      f = @showFields()
      t = _.keys @groups
      d = _.difference t, f
      unless _.isEmpty d
        @dynamic.push d[0]

  changeHistory: (f...) =>
    # move fields to first position
    @history = _.without @history, f
    @history = _.union f, @history


module.exports =

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
        # we don't need some fields combined into groups, if we have
        # exact match, we don't need tham in the table, and we may not get tham
        # in resutlt at all, so just ignore this case
        if _.isUndefined fh[o.model][o.name] and
            (f.meta.search.matchType == "MatchExact" or
             f.meta.search.matchType == "MatchRefExist")
          continue
        # if we can't ignore field and have to draw something in the table
        # but don't have such field in given model, we better fail
        # during init
        else if _.isUndefined fh[o.model][o.name]
          throw new Error "Can't make field group, can't find" +
            " field for #{o.model}.#{o.name}"
        else
          groups[f.name][o.model].push fh[o.model][o.name]

      labels[f.name] = f.meta.label if f.meta?.label?
    return { labels: labels, groups: groups }
