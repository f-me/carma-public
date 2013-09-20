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
          search: "full"
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

  showFields = (model, flds) ->
    ko.observableArray _.filter model.fields, (f) -> _.contains flds, f.name

  window.addField = (flds, fldName) ->
    fs = _.filter model.fields, (f) -> f.name == fldName
    return unless fs
    flds.push(f) for f in fs

  window.delField = (flds, fldName) ->
    fs = _.filter model.fields, (f) -> f.name == fldName
    return unless fs
    flds.removeAll(fs)

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

    rr =
      model     : model
      kvms      : [kvm1, kvm2]
      showFields: showFields model, ['search', 'city', 'isDealer']

      searchSortASC: () ->
        console.log "Started searchSortASC function"
        @findField 'contact'

      searchSortDSC: () ->
        console.log "Started searchSortDSC function"

      citySortASC: () ->
        console.log "Started citySortASC function"

      citySortDSC: () ->
        console.log "Started citySortDSC function"

      addField: (name, replace = true) ->
        exists = _.some @showFields(), (f) -> f.name is name
        unless exists
          # if 'replace' is 'true' - put new field in place to last one
          @showFields.pop() if replace
          @showFields.push _.find model.fields, (f) -> f.name is name

      removeField: (name) ->
        @showFields.remove (f) -> f.name is name

      findField: (name) ->
        field = _.find model.fields, (f) -> f.name is name
        searchType = field.meta.search
        switch searchType
          when 'full' then @removeField name
          when 'fuzzy' then @addField name
          else throw new Error "Unknown model.field.meta.search=#{searchType}"

    global.rr = rr
    ko.applyBindings(rr, $("#tbl")[0])

  template: tpl
