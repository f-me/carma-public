define [], () ->

  class MuTable
    constructor: (opts) ->

      # max number of fields in table
      # it affects to the field replacement
      @maxFieldNum = opts.maxFieldNum or 8

      # fields what can't be replaced
      @nonReplacedFields = opts.nonReplacedFields or []

      # filds displayed by default
      @originFields = opts.originFields or []

      # replaced fields
      @hiddenFields = []

      unless opts.searchKVM or opts.searchKVM?._meta.model
        throw new Error "Invalid value for searchKVM=#{opts.searchKVM}"
      @searchKVM = opts.searchKVM
      @model = @searchKVM._meta.model
      do @bindModel
      do @bindSearchKVM

    bindModel: ->
      # currently showing fields
      @showFields = ko.observableArray _.filter @model.fields, (f) =>
        _.contains @originFields, f.name

    bindSearchKVM: ->
      _.each @model.fields, (f) =>
        @searchKVM[f.name].subscribe (newVal) =>
          if newVal
            @processField f.name
          else
            # nothing to search in this field
            # it unused now, replace it
            @removeField f.name
            unless @tableFull() or _.isEmpty @hiddenFields
              @addField @hiddenFields.pop(), false

    addField: (name, replace = true) ->
      exists = _.some @showFields(), (f) -> f.name is name
      unless exists
        # if 'replace' is 'true' - put new field in place to first one
        # but firstly try to replace one of the @originFields
        if replace
          @hideField @nextReplaceField()
        field = _.find @model.fields, (f) -> f.name is name
        @showFields.push field if field

    addFields: (fields...) ->
      _.each fields, (name) => @addField name, @tableFull()

    removeField: (name) ->
      @showFields.remove (f) -> f.name is name

    removeFields: (fields...) ->
      @showFields.remove (f) -> _.contains fields, f.name

    hideField: (name) ->
      @hiddenFields.push name
      @removeField name

    processField: (name) ->
      field = _.find @model.fields, (f) -> f.name is name
      searchType = field.meta.search
      switch searchType
        when 'full' then @hideField name
        when 'fuzzy' then @addField name, @tableFull()
        else throw new Error "Unknown model.field.meta.search=#{searchType}"

    tableFull: ->
      @showFields().length >= @maxFieldNum

    nextReplaceField: ->
      showingFields = _.pluck @showFields(), 'name'
      origins = _.intersection @originFields, showingFields
      origins = _.without origins, @nonReplacedFields
      if _.isEmpty origins
        _.first showingFields
      else
        _.first origins

  MuTable: MuTable

