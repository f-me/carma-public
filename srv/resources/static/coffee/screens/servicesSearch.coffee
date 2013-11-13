define [ "utils"
       , "model/main"
       , "model/utils"
       , "search/model"
       , "search/utils"
       , "screens/servicesSearch/model"
       , "text!tpl/screens/servicesSearch.html"
       , "json!/cfg/model/Case?view=search"
       , "sync/servicesSearch"
       , "lib/state-url"
       ], ( utils
          , main
          , mutils
          , smodel
          , SUtils
          , ssmodels
          , tpl
          , model
          , sync
          , State) ->

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

  constructor: ->
    searchKVM = main.buildKVM model, {}

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

    searchKVM.searchResults = SUtils.mkResultObservable ssmodels

    q = new sync.ServicesSearchQ(searchKVM)
    searchKVM._meta.q = q

    ko.applyBindings { kvm: searchKVM, wrapFields: "search-wrap"},
                     $("#search-conditions")[0]

    ko.applyBindings { kvm: searchKVM, f: _.last(searchKVM._meta.model.fields) },
                     $("#show-field")[0]

    # all about results
    tg = smodel.transformFields searchKVM, ssmodels
    rfields = smodel.mkFieldsDynView searchKVM, tg,
      [ { name: 'Case_id', fixed: true }
      , { name: 'city'    }
      , { name: 'vin' }
      , { name: 'program' }
      ]

    ctx =
      kvms: ko.sorted
        kvms: searchKVM.searchResults
        sorters: mutils.buildSorters(model)
      showFields: rfields
      searchKVM: searchKVM

    ko.applyBindings ctx, $("#search-results")[0]

    State.load State.statify searchKVM

  template: tpl
