define [ "utils"
       , "model/main"
       , "text!tpl/screens/servicesSearch.html"
       , "lib/resultsTable"
       ], (utils, main, tpl, rt) ->

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

    rr = new rt.ResultsTable {model: model
                            , searchKVM: searchKVM
                            , maxFieldNum: 4
                            , originFields: ['search', 'city', 'isDealer']
                            , nonReplacedFields: ['caseId']
                              }
    rr.searchSortASC = ->
      console.log "Started searchSortASC function"
    rr.searchSortDSC = ->
      console.log "Started searchSortDSC function"
    rr.citySortASC = ->
      console.log "Started citySortASC function"
    rr.citySortDSC = ->
      console.log "Started citySortDSC function"

    rr.rows = [kvm1, kvm2]

    global.rr = rr
    ko.applyBindings(rr, $("#tbl")[0])

  template: tpl
