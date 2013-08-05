define [ "utils"
       , "model/main"
       , "text!tpl/screens/servicesSearch.html"
       ], (utils, main, tpl) ->

  model =
    name: "partnerSearch"
    title: "Экран поиска партнеров"
    fields: [
      { name: "search"
      , meta: { label: "Поиск", nosearch: true }
      },
      { name: "city"
      , type: "dictionary"
      , meta:
          dictionaryName: "DealerCities"
          label: "Город"
      },
      { name: "isDealer"
      , type: "checkbox"
      , meta: { label: "Дилер" }
      },
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
    f = _.find model.fields, (f) -> f.name == fldName
    return unless f
    flds.push(f)

  constructor: ->
    kvm1 = main.buildKVM model,
      fetched:
        search  : "qwqweqwe"
        city    : "Moskva"
        isDealer: true
    setTpls kvm1

    kvm2 = main.buildKVM model,
      fetched:
        search  : "111"
        city    : "Sankt-Peterburg"
        isDealer: false
    setTpls kvm2

    rr =
      model     : model
      kvms      : [kvm1, kvm2]
      showFields: showFields model, ['search', 'city']
    global.rr = rr
    ko.applyBindings(rr, $("#tbl")[0])

  template: tpl