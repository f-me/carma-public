define [ "utils"
       , "model/main"
       , "model/utils"
       , "search/model"
       , "screens/servicesSearch/model"
       , "text!tpl/screens/servicesSearch.html"
       , "json!/cfg/model/Case?view=search"
       , "json!/cfg/model/Case"
       , "sync/servicesSearch"
       ], ( utils
          , main
          , mutils
          , smodel
          , ssmodel
          , tpl
          , model
          , kase
          , sync) ->

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

    tg = smodel.transformFields searchKVM, [kase]
    rfields = smodel.mkFieldsDynView searchKVM, tg,
      [ { model: 'Case', name: 'id', fixed: true }
      , { model: 'Case', name: 'city'    }
      , { model: 'Case', name: 'car_vin' }
      , { model: 'Case', name: 'program' }
      ]
    console.log tg

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



    fnames = ko.observableSet( [ "showField"
                               , "car_vin"
                               , "callDate"
                               , "caseid"
                               , "phone"
                               , "car_plateNum"
                               , "caseAddress_address"
                               , "city"
                               ] )
    searchKVM._meta.showFields = ko.computed
      read: ->  _.filter searchKVM._meta.model.fields,
                        (f) -> _.contains fnames(), f.name
      write: (v) -> fnames(v)

    ko.applyBindings searchKVM, $("#search-conditions")[0]

  template: tpl
