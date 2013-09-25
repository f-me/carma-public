define [ "utils"
       , "model/main"
       , "text!tpl/screens/servicesSearch.html"
       ], (utils, main, tpl) ->

  model =
    name: "partnerSearch"
    title: "Экран поиска партнеров"
    fields: [
      { name: "search"
      , meta:
          label: "Поиск"
          nosearch: true
      },
      { name: "contact"
      , meta:
          label: "ФИО"
          search: "fuzzy"
      },
      { name: "callDate"
      , meta:
          label: "Дата и время"
          search: "fuzzy"
      },
      { name: "city"
      , type: "dictionary"
      , meta:
          dictionaryName: "DealerCities"
          label: "Город"
          search: "full"
      },
      { name: "isDealer"
      , type: "checkbox"
      , meta:
          label: "Дилер"
      }
    ]

  setTpls = (kvm) ->
    kvm._meta.tpls = {}
    for f in kvm._meta.model.fields
      type  = f.type
      type ?= "text"
      tpl = $("##{type}-txt-template").html()
      unless tpl
        throw new Error("Can't find template for #{type}
   in #{kvm._meta.model.name}")
      kvm._meta.tpls[f.name] = tpl

  constructor: ->
    kvm1 = main.buildKVM model,
      fetched:
        search  : "qwqweqwe"
        contact : "Stan"
        callDate: "1379591833"
        city    : "Moskva"
        isDealer: true
    setTpls kvm1

    kvm2 = main.buildKVM model,
      fetched:
        search  : "111"
        contact : "Kenny"
        callDate: "1379551233"
        city    : "Sankt-Peterburg"
        isDealer: false
    setTpls kvm2

    searchKVM = main.buildKVM model, {}
    ko.applyBindings(searchKVM, $("#search-section")[0])

    class TableResult
      constructor: ->
        @model = model
        @kvms = [kvm1, kvm2]

        # max number of fields in table
        # it affects to the field replacement
        @MAX_FIELD_NUM = 4

        # fields what can't be replaced
        @NON_REPLACED_FIELDS = ['caseId']

        # filds displayed by default
        @originFields = ['search', 'city', 'isDealer']

        # currently showing fields
        @showFields = ko.observableArray _.filter @model.fields, (f) =>
          _.contains @originFields, f.name

        # replaced fields
        @hiddenFields = []

        @searchKVM = searchKVM
        @bindSearchKVM()

      searchSortASC: ->
        console.log "Started searchSortASC function"

      searchSortDSC: ->
        console.log "Started searchSortDSC function"

      citySortASC: ->
        console.log "Started citySortASC function"

      citySortDSC: ->
        console.log "Started citySortDSC function"

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

      findField: (name) ->
        field = _.find @model.fields, (f) -> f.name is name
        searchType = field.meta.search
        switch searchType
          when 'full' then @hideField name
          when 'fuzzy' then @addField name, @tableFull()
          else throw new Error "Unknown model.field.meta.search=#{searchType}"

      tableFull: ->
        @showFields().length >= @MAX_FIELD_NUM

      nextReplaceField: ->
        showingFields = _.pluck @showFields(), 'name'
        origins = _.intersection @originFields, showingFields
        origins = _.without origins, @NON_REPLACED_FIELDS
        if _.isEmpty origins
          _.first showingFields
        else
          _.first origins

      bindSearchKVM: ->
        _.each @model.fields, (f) =>
          @searchKVM[f.name].subscribe (newVal) =>
            if newVal
              @findField f.name
            else
              # nothing to search in this field
              # it unused now, replace it
              @removeField f.name
              unless @tableFull() or _.isEmpty @hiddenFields
                @addField @hiddenFields.pop(), false

    rr = new TableResult
    global.rr = rr
    ko.applyBindings(rr, $("#tbl")[0])

  template: tpl
