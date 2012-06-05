#/ Transfrom model definitions into Backbone models, render model
#/ forms, template helpers.

# Backbonize a model
#
# @return Constructor of Backbone model

this.backbonizeModel = (models, modelName) ->
  defaults         = {}
  fieldHash        = {}
  dictionaryFields = []
  referenceFields  = []
  requiredFields   = []
  regexpFields     = []
  groups           = []

  model = models[modelName]

  for f in model.fields
    if f.meta?
      requiredFields.push(f.name) if f.meta.required
      regexpFields.push(f.name)   if _.has(f.meta, "regexp")

    fieldHash[f.name] = f
    defaults[f.name]  = null

    referenceFields.push(f.name)  if f.type == "reference"
    dictionaryFields.push(f.name) if f.type == "dictionary"
    groups.push(f.groupName)      if f.groupName? and f.groupName not in groups

  M = Backbone.Model.extend
    defaults: defaults

    # Field caches

    # List of fields with dictionary type
    dictionaryFields: dictionaryFields
    # List of field names which hold references to different
    # models.
    referenceFields: referenceFields
    # List of required fields
    requiredFields: requiredFields
    # List of fields with regexp checks
    regexpFields: regexpFields
    # List of groups present in model
    groups: groups

        # Temporary storage for attributes queued for sending to
    # server.
    attributeQueue: {}
    # attributeQueue backuped before saving to server.
    # If save fails we merge new changes with backupped ones.
    # This prevents data loss in case of server failures.
    attributeQueueBackup: {}
    initialize: ->
      if not this.isNew() then this.fetch()
      setTimeout((=> this.setupServerSync()), 1000)

    # Original definition
    #
    # This model and Backbone model (which is actually a
    # representation of instance of original model) are not to be
    # confused!
    model: model
    # Name of model definition
    name: modelName
    # Readable model title
    title: model.title
    # Hash of model fields as provided by model definition.
    fieldHash: fieldHash
    # Bind model changes to server sync
    setupServerSync: ->
      realUpdates = ->
            # Do not resave model when id is set after
            # first POST
            #
            # TODO Still PUT-backs
            this.save() unless this.hasChanged("id")
      this.bind("change", _.throttle(realUpdates, 500), this)
    set: (attrs, options) ->
      Backbone.Model.prototype.set.call(this, attrs, options)
      # Push new values in attributeQueue
      #
      # Never send "id", never send anything if user has no
      # canUpdate permission.
      #
      # Note that when model is first populated with data from
      # server, all attributes still make it to the queue,
      # resulting in full PUT-back upon first model "change"
      # event.
      #
      # TODO _.extend doesn't work here
      for k of attrs when k isnt 'id' and
          _.has(this.fieldHash, k)    and
          this.model.canUpdate        and
          this.fieldHash[k].canWrite  and
          not _.isNull(attrs[k])
        this.attributeQueue[k] = attrs[k]

      # Do not send empty updates to server
    save: (attrs, options) ->
      if not _.isEmpty(this.attributeQueue)
        options = if options then _.clone(options) else {}

        error = options.error
        options.error = (model, resp, options) ->
          _.isFunction(error) and error(model, resp, options)
          _.defaults(this.attributeQueue, this.attributeQueueBackup)

        Backbone.Model.prototype.save.call(this, attrs, options)

    # For checkbox fields, translate "0"/"1" to false/true
    # boolean.
    parse: (json) ->
      m = this.model;
      for k of json
        # TODO Perhaps inform client when unknown field occurs
        if (k isnt "id") and _.has(this.fieldHash, k)
          type = this.fieldHash[k].type
          if type.match(/^date/) and json[k].match(/\d+/)
            format = if type == "date"
                "dd.MM.yyyy"
              else
                 "dd.MM.yyyy HH:mm:ss"
            json[k] = new Date(json[k] * 1000).toString(format)
          if type == 'reference'
            setReference this, json, k, models
          else
            if (type == "checkbox")
              json[k] = json[k] == "1"
      return json

    toJSON: ->
      # Send only attributeQueue instead of the whole object
      json = _.clone(this.attributeQueue)
      this.attributeQueueBackup = _.clone(json)
      this.attributeQueue = {}
      for k of json
        if _.has(this.fieldHash, k)
          # serialize date field to unixtime (am I wrong?)
          if this.fieldHash[k].type.match(/^date/)
            date = Date.parseExact(
              json[k],
              ["dd.MM.yyyy HH:mm:ss", "dd.MM.yyyy"])
            if date
              timestamp = Math.round(date.getTime() / 1000)
              json[k] = String(timestamp)
          # serialize references to name1:id1,name2:id2,... string
          if this.fieldHash[k].type == 'reference'
            json[k] = ("#{r.name}:#{r.id}" for r in json[k]).join(',')
        # Map boolean values to string "0"/"1"'s for server
        # compatibility
        if _.isBoolean(json[k])
          json[k] = String(if json[k] then "1" else "0")
      return json

    urlRoot: "/_/" + modelName

  return M

setReference = (parent, json, field, models) ->
  return json[field] = [] unless json[field]?
  references = for m in json[field].split ','
    [name, id] = (v.trim() for v in m.split(':'))
    mkBb = backbonizeModel(models, name)
    new mkBb({id:id})
  # genRefAccessors(parent, field, references)
  json[field] = references

# genRefAccessors = (bindInst, field, refs) =>
#   tp = (f) -> _.chain(f v).tap(bindInst.trigger("change:#{@field}")).value()
#   genFor = (name, fun) ->
#     bindInst["#{name}#{ucfirst field}"] = fun
#   set = (refs) -> bindInst.set(field, refs)
#   get =        -> bindInst.get(field)

#   genFor 'get',        -> get field
#   genFor 'set', set
#   genFor 'add', (refs) -> set(_.union get(), toA refs)
#   genFor 'rem', (refs) -> set _.reject get(),
#     (r) -> if _.any(refs, (c) -> c.id == r.id and c.name == r.name) then true

# ucfirst = (str) -> str.replace /(\b)([a-zA-Z])/, (l) -> l.toUpperCase()


