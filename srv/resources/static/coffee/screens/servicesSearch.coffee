define [ "utils"
       , "model/main"
       , "model/utils"
       , "search/model"
       , "search/utils"
       , "search/pager"
       , "screens/servicesSearch/model"
       , "text!tpl/screens/servicesSearch.html"
       , "json!/cfg/model/Case?view=search"
       , "json!/cfg/model/Service?view=search"
       , "json!/cfg/model/Towage?view=search"
       , "sync/servicesSearch"
       , "lib/state-url"
       ], ( utils
          , main
          , mutils
          , smodel
          , SUtils
          , SPager
          , ssmodels
          , tpl
          , caseModel
          , servicesModel
          , towageModel
          , sync
          , State) ->

  model = $.extend true, {}, caseModel
  model.fields = model.fields
    .concat(servicesModel.fields)
    .concat(towageModel.fields)

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

    SPager.buildPager searchKVM
    ko.applyBindings searchKVM._meta.pager, $(".pager")[0]

    searchKVM.showFields.set = (fs) ->
      searchKVM.showFields( _.filter searchKVM._meta.model.fields,
                            (f) -> _.contains fs, f.name)

    searchKVM.showFields.del = (fs) ->
      searchKVM[fs.name](null)
      searchKVM.showFields( _.reject searchKVM.showFields(),
                           (f) -> _.contains fs, f.name)

    searchKVM.showFields.set(
                          [ "callDate"
                            "createTime"
                            "Case_id"
                            "phone"
                            "contact"
                            "vin"
                            "plateNum"
                          ] )

    searchKVM.searchResults = SUtils.mkResultObservable searchKVM, ssmodels

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
      , { name: 'contact'       }
      , { name: 'callDate'      }
      , { name: 'phone' }
      , { name: 'plateNum'  }
      , { name: 'vin'       }
      , { name: 'program'       }
      ]

    ctx =
      kvms: ko.sorted
        kvms: searchKVM.searchResults
        sorters: mutils.buildSorters(model)
      showFields: rfields
      searchKVM: searchKVM

    ko.applyBindings ctx, $("#search-results")[0]

    state = {kvm: searchKVM, pager: searchKVM._meta.pager }
    State.load state

    State.persistKVM 'kvm', state
    searchKVM._meta.pager.offset.subscribe -> State.save state

  template: tpl
