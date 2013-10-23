define [ "utils"
       , "model/main"
       , "model/utils"
       , "search/model"
       , "screens/servicesSearch/model"
       , "text!tpl/screens/servicesSearch.html"
       , "json!/cfg/model/Case?view=search"
       , "sync/servicesSearch"
       ], ( utils
          , main
          , mutils
          , smodel
          , ssmodels
          , tpl
          , model
          , sync) ->

  model.fields = model.fields.concat [
    {
    , name: "showFields"
    , meta:
        noadd: true
        nosearch: true
    },
    { name: "fieldsList"
    , type: "dictionary"
    , meta:
        label: "Добавить критерий поиска"
        noadd: true
        nosearch: true
        dictionaryType: "HiddenFieldsDict"
    }
  ]

  setTpls = (kvm) ->
    kvm._meta.tpls = {}
    for f in kvm._meta.model.fields
      type  = f.type
      type ?= "text"
      tpl = $("##{type}-txt-template").html()
      return unless tpl
      kvm._meta.tpls[f.name] = tpl

  constructor: ->
    searchKVM = main.buildKVM model, {}

    tg = smodel.transformFields searchKVM, ssmodels
    rfields = smodel.mkFieldsDynView searchKVM, tg,
      [ { model: 'Case', name: 'id', fixed: true }
      , { model: 'Case', name: 'city'    }
      , { model: 'Case', name: 'car_vin' }
      , { model: 'Case', name: 'program' }
      ]

    ctx =
      kvms: ko.sorted { kvms: [], sorters: mutils.buildSorters(model)}
      showFields: rfields
      searchKVM: searchKVM

    ko.applyBindings ctx, $("#search-results")[0]

# Кейс
# Телефон
# Госномер
# Контакт
# VIN
# Карта участника
# Адрес места поломки
# Дата звонка
# Город
# Сотрудник принявший звонок
# Услуга
# Марка
# Модель
# Партнёр
# Дилер, куда эвакуируют автомобиль
# Программа
# Что случилось?
# Неисправность со слов клиента

    searchKVM.showFields.set = (fs) ->
      searchKVM.showFields( _.filter searchKVM._meta.model.fields,
                            (f) -> _.contains fs, f.name)

    searchKVM.showFields.del = (fs) ->
      searchKVM.showFields( _.reject searchKVM.showFields(),
                           (f) -> _.contains fs, f.name)

    searchKVM.showFields.set(
                          [ "car_vin"
                          , "callDate"
                          , "caseid"
                          , "phone"
                          , "car_plateNum"
                          , "caseAddress_address"
                          , "city"
                          ] )


    ko.applyBindings { kvm: searchKVM, wrapFields: "search-wrap"},
                     $("#search-conditions")[0]

    ko.applyBindings { kvm: searchKVM, f: _.last(searchKVM._meta.model.fields) },
                     $("#show-field")[0]

  template: tpl
