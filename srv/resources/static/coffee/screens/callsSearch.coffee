define [ "utils"
       , "model/main"
       , "model/utils"
       , "search/model"
       , "search/utils"
       , "search/pager"
       , "text!tpl/screens/search.html"
       , "json!/cfg/model/Call"
       , "json!/cfg/model/Call?view=search"
       , "sync/search"
       , "lib/state-url"
       ], ( utils
          , main
          , mutils
          , smodel
          , SUtils
          , SPager
          , tpl
          , callModel
          , callSearchModel
          , sync
          , State) ->

  allModels = arrToObj 'name', [callModel]

  resultFields =
    Call: [
      "callDate"
      "callerName_phone1"
      "callerName_phone2"
      "callerName_phone3"
      "callerName_phone4"
      "callerName_ownerPhone1"
      "callerName_ownerPhone2"
      "callerName_ownerPhone3"
      "callerName_ownerPhone4"
      "callerName_name"
      "callerName_ownerName"
      "program"
      "wazzup"
      "callTaker"
      "callType"
      ]

  all = $.extend true, {}, allModels
  for n,m of all
    m.fields = _.filter m.fields, (f) -> _.contains resultFields[n], f.name
  arrToObj 'name', all

  callSearchModel.fields = callSearchModel.fields.concat [
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

  model = callSearchModel

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

    searchKVM.showFields.set([ "phone"
                               "callDate"
                               "callTaker"
                               "callType"
                               "program"
                               "wazzup"
                            ])

    searchKVM.searchResults = SUtils.mkResultObservable searchKVM, all

    q = new sync.searchQ searchKVM,
      apiUrl: "/search/call"
      defaultSort:
        fields: [ { model: "Call", name: "id" } ]
        order: "desc"
    searchKVM._meta.q = q

    ko.applyBindings { kvm: searchKVM, wrapFields: "search-wrap"},
                     $("#search-conditions")[0]

    ko.applyBindings { kvm: searchKVM
                     , f: _.last(searchKVM._meta.model.fields)
                     },
                     $("#show-field")[0]

    # all about results
    tg = smodel.transformFields searchKVM, all
    rfields = smodel.mkFieldsDynView searchKVM, tg,
      [ { name: 'phone',    fixed: true }
      , { name: 'callDate', fixed: true }
      , { name: 'program'               }
      , { name: 'wazzup'                }
      , { name: 'callTaker'             }
      ]

    ctx =
      kvms: searchKVM.searchResults
      showFields: rfields
      searchKVM: searchKVM

    searchKVM.searchResults.set_sorter =
     (name, ord) -> searchKVM.sort(name, ord)

    searchKVM.sort = (fname, ord) ->
      fs = arrToObj 'name', searchKVM._meta.model.fields, (v) ->
        v.meta.search?.original
      searchKVM._meta.q.sort(fs[fname], ord)

    ko.applyBindings ctx, $("#search-results")[0]

    state = { kvm: searchKVM, pager: searchKVM._meta.pager }
    State.load state

    State.persistKVM 'kvm', state
    searchKVM._meta.pager.offset.subscribe -> State.save state

  template: tpl
