define ["model/main"
      , "model/utils"
      , "utils"
      , "sync/crud"
      , "sync/datamap"]
    , (Main, ModelUtils, Utils, Crud, DataMap) ->

  ko.bindingHandlers.renderRow =
    update: (el, acc, allBindigns, fld, ctx) ->
      _.each ctx.$parent.columns, (c) ->
        tplid = c.meta?.widget || c.type || 'text'
        tplid = "dictionary-many" if /^dictionary-set/.test(c.type)
        tplid = "text" if c.type == "ident"
        tpl = Mustache.render $("##{tplid}-table-template").html(), c
        td = document.createElement("td")
        el.appendChild(td)
        ko.utils.setHtml td, tpl

  class Table
    constructor: (opt) ->
      @limitDef = 10
      @offsetDef = 0

      @limit = ko.observable(@limitDef)
      @offset = ko.observable(@offsetDef)

      @dataModel = global.model(opt.dataModel)
      @columns = _.map opt.columns, (c) =>
        _.find @dataModel.fields, (f) =>
          f.name is c
      @items = ko.observableArray()

      @clickCb = []

      @typeahead = ko.observable()
      @typeaheadK = ko.computed( =>
        @resetPager()
        @typeahead()).extend {throttle: 300}

      @kvms = ko.sorted
        kvms:
          @items
        sorters:
          ModelUtils.buildSorters @dataModel
        filters:
          typeahead: (v) =>
            return true unless @typeaheadK()
            Utils.kvmCheckMatch @typeaheadK(), v
      @kvms.change_filters ['typeahead']

      @rows = ko.computed =>
        @kvms().slice(@offset(), @offset() + @limit())

      @prev = ko.computed =>
        offset = @offset() - @limit()
        if offset < 0 then null else offset / @limit() + 1

      @next = ko.computed =>
        length = @kvms().length
        offset = @offset() + @limit()
        if (length - offset) > 0 then offset / @limit() + 1 else null

      @page = ko.computed =>
        @offset() / @limit() + 1

      @fetchData()


    fetchData: =>
      $.getJSON "/_/#{@dataModel.name}", (data) =>
        @setData data

    setData: (data) =>
      @data = data
      @items.removeAll()
      mapper = new DataMap.Mapper(@dataModel)
      kvms = _.map @data, (d) =>
        k = Main.buildKVM @dataModel, {fetched: mapper.s2cObj d}
        k._meta.q = new Crud.CrudQueue(k, k._meta.model, {not_fetch: true})
        k
      @items(kvms)

    resetPager: =>
      @limit(@limitDef)
      @offset(@offsetDef)

    prevPage: =>
      @offset(@offset() - @limit())

    nextPage: =>
      @offset(@offset() + @limit())

    onClick: (cb) =>
      @clickCb.push cb

    rowClick: (row) =>
      return =>
        rowData = _.find @data, (d) ->
          d.id is parseInt row.id()
        _.each @clickCb, (cb) ->
          cb(rowData)

  Table
