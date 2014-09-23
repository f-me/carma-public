define [ "utils"
       , "model/main"
       , "model/utils"
       , "search/screen"
       , "search/model"
       , "search/utils"
       , "search/pager"
       , "sync/search"
       , "lib/state-url"
       , "text!tpl/fields/form.html"
       ], ( utils
          , main
          , mutils
          , Screen
          , smodel
          , SUtils
          , SPager
          , sync
          , State
          , Tpl) ->

  filterModels = (allowed, models) ->
    mdls = for m in  models
      mdl = $.extend true, {}, m
      mdl.fields = _.filter m.fields, (f) ->
         _.contains allowed[m.name], f.name
      mdl
    arrToObj 'name', mdls

  mergeFields = (ms) ->
    m = $.extend true, {}, ms[0]
    m.fields = _.union.apply @, _.pluck ms, 'fields'
    m


  constructor: (opts)->

    # need this hack because renderField can't work with require js
    $("#layout").append $("<div style='display: none;'/>").html(Tpl)

    allModels = filterModels opts.allowedResultFields, opts.resultModels

    searchModel = mergeFields opts.searchModels
    searchModel.fields = searchModel.fields.concat [
      { name: "showFields"
      , meta:
          noadd: true
          nosearch: true
      },
      { name: "fieldsList"
      , type: "dictionary"
      , meta:
          label: "Добавить критерий поиска"
          noadd: true
          invisible: opts?.hideFieldsList
          nosearch: true
          dictionaryType: "HiddenFieldsDict"
      }]

    searchKVM = main.buildKVM searchModel, {}

    SPager.buildPager searchKVM
    ko.applyBindings searchKVM._meta.pager, $(".pager")[0]

    searchKVM.showFields.set = (fs) ->
      searchKVM.showFields _.filter searchKVM._meta.model.fields, (f) ->
        _.contains fs, f.name

    searchKVM.showFields.del = (fs) ->
      searchKVM[fs.name](null)
      searchKVM.showFields _.reject searchKVM.showFields(), (f) ->
        _.contains fs, f.name

    searchKVM.showFields.set opts.searchFields

    searchKVM.searchResults = SUtils.mkResultObservable searchKVM, allModels

    q = new sync.searchQ searchKVM,
      apiUrl: opts.apiUrl
      defaultSort: opts.defaultSort
      searchHook: opts.searchHook
    searchKVM._meta.q = q

    console.log opts.predFieldWrap
    ko.applyBindings { kvm: searchKVM
                     , wrapFields: (opts.predFieldWrap || "search-wrap")
                     },
                     $("#search-conditions")[0]

    ko.applyBindings { kvm: searchKVM
                     , f: _.last(searchKVM._meta.model.fields)
                     },
                     $("#show-field")[0]

    tg = smodel.transformFields searchKVM, allModels
    rfields = smodel.mkFieldsDynView searchKVM, tg, opts.resultTable
    searchKVM.resultFields = rfields

    ctx =
      kvms: searchKVM.searchResults
      showFields: rfields
      searchKVM: searchKVM

    searchKVM.searchResults.set_sorter = (name, ord) ->
      searchKVM.sort(name, ord)

    searchKVM.sort = (fname, ord) ->
      fs = arrToObj 'name', searchKVM._meta.model.fields, (v) ->
        v.meta.search?.original
      searchKVM._meta.q.sort(fs[fname], ord)

    ko.applyBindings ctx, $("#search-results")[0]

    if !opts.noState?
      state = { kvm: searchKVM, pager: searchKVM._meta.pager }
      State.load state

      State.persistKVM 'kvm', state
    searchKVM._meta.pager.offset.subscribe -> State.save state

    searchKVM
