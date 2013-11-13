define [ "utils"
       , "model/main"
       , "model/utils"
       , "search/model"
       , "screens/servicesSearch/model"
       , "text!tpl/screens/servicesSearch.html"
       , "json!/cfg/model/Case?view=search"
       , "sync/servicesSearch"
       , "lib/state-url"
       ], ( utils
          , main
          , mutils
          , smodel
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

    robs = ko.observable([])
    searchKVM.searchResults = ko.computed
      read: -> robs()
      write: (v) -> robs(buildKVMS ssmodels, fixNames ssmodels, v)

    # We receiving all fieldnames in lowercase (at least for now)
    # so we have to translate them into normal ones according to
    # their model
    fixNames = (ssmodels, v) -> _.map v, (v) -> fixName ssmodels, v

    fixName = (models, rawInst) ->
      fixed = {}
      for m, fs of models
        fnames = _.pluck fs.fields, 'name'
        fields = {}
        for fn in fnames
          fields[fn] = rawInst[m.toLowerCase()][fn.toLowerCase()]
        fixed[m] = fields
      return fixed

    buildKVMS = (models, raws) -> _.map raws, (r) -> buildKVM models, r

    buildKVM = (models, rs) ->
      r = {}
      for n, m of models
        r[n] = main.buildKVM m, { fetched: rs[n] }
      return r

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
