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

      # fields currently in search
      @inSearchFields = []

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
      for f in @model.fields when f.meta?.search and not f.meta?.nosearch
        do (f) =>
          @searchKVM[f.name].subscribe (newVal) =>
            if newVal
              @inSearchFields.push f.name
              @processField f.name
            else
              # nothing to search in this field
              # it unused now, replace it
              if (index = @inSearchFields.indexOf f.name) > -1
                @inSearchFields.splice index, 1
              @removeField f.name
              unless @tableFull() or _.isEmpty @hiddenFields
                @addField @lastHiddenField(), false

    addField: (name, replace = true) ->
      exists = _.some @showFields(), (f) -> f.name is name
      unless exists
        # if 'replace' is 'true' - put new field in place to first one
        # but firstly try to replace one of the @originFields
        if replace
          @hideField @nextReplaceField()
        if field = (_.find @model.fields, (f) -> f.name is name)
          # if field has searchFields add them to table
          # otherwise add the field itself
          sfNames = @searchFields name
          unless _.isEmpty sfNames
            @addFields sfNames
          else
            @showFields.push field

    addFields: (fields...) ->
      fields = _.flatten fields
      _.each fields, (name) => @addField name, @tableFull()

    removeField: (name) ->
      names = @searchFields name
      unless _.isEmpty names
        @showFields.remove (f) -> _.contains names, f.name
      else
        @showFields.remove (f) -> f.name is name

    removeFields: (fields...) ->
      _.each fields, (name) => @removeField name

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

    searchFields: (name) ->
      field = _.find @model.fields, (f) -> f.name is name
      field?.meta?.searchFields or []

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

    lastHiddenField: ->
      hiddens = _.without @hiddenFields, @inSearchFields
      last = _.first hiddens
      @hiddenFields = _.without @hiddenFields, last
      last

  MuTable: MuTable
