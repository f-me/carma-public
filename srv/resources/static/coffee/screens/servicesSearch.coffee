define [ "utils"
       , "model/main"
       , "model/utils"
       , "dictionaries/muTable"
       , "text!tpl/screens/servicesSearch.html"
       ], (utils, main, mutils, muTable, tpl) ->

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
          searchFields: ['callDateDay', 'callDateYear']
      },
      { name: "callDateDay"
      , meta:
          label: "День"
      },
      { name: "callDateYear"
      , meta:
          label: "Год"
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
      { name: "showField"
      , type: "dictionary"
      , meta:
          label: "Добавить критерий поиска"
          noadd: true
          dictionaryType: "HiddenFieldsDict"
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
    date1 = new Date 1380193258500
    kvm1 = main.buildKVM model,
      fetched:
        search  : "qwqweqwe"
        contact : "Stan"
        callDate: do date1.getTime
        callDateDay : do date1.getDay
        callDateYear: do date1.getFullYear
        city    : "Moskva"
        isDealer: true
    setTpls kvm1

    date2 = new Date 1380293358500
    kvm2 = main.buildKVM model,
      fetched:
        search  : "111"
        contact : "Kenny"
        callDate: do date2.getTime
        callDateDay : do date2.getDay
        callDateYear: do date2.getFullYear
        city    : "Sankt-Peterburg"
        isDealer: false
    setTpls kvm2

    searchKVM = main.buildKVM model, {}

    global.k = searchKVM

    mutableFields = new muTable.MuTable({
      , searchKVM: searchKVM
      , maxFieldNum: 4
      , originFields: ['search', 'city', 'isDealer']
      , nonReplacedFields: ['caseId']
    }).showFields

    ctx =
      kvms: ko.sorted { kvms: [kvm1, kvm2], sorters: mutils.buildSorters(model)}
      showFields: mutableFields
      searchKVM: searchKVM

    ko.applyBindings ctx, $("#search-results")[0]

    fnames = ko.observableSet(["showField", "contact", "callDate"])
    searchKVM._meta.showFields = ko.computed
      read: ->  _.filter searchKVM._meta.model.fields,
                        (f) -> _.contains fnames(), f.name
      write: (v) -> fnames(v)

    ko.applyBindings searchKVM, $("#search-conditions")[0]


  template: tpl
