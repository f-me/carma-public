define "tableman", ["utils"], (utils) ->
  class Table
    constructor: (params) ->
      {@tableName, @objURL} = params
      @sLength = "dataTables_length form-inline"
      @sFilter = "dataTables_filter form-inline"
      @objsToRows = null
      @dataTable = null
      @dataTableOpts = null
      @$tableEl = null

    show: ->
      $.fn.dataTableExt.oStdClasses.sLength = @sLength
      $.fn.dataTableExt.oStdClasses.sFilter = @sFilter

      @$tableEl = $("##{@tableName}-table")
      unless @$tableEl.hasClass "dataTable"
        @dataTable = utils.mkDataTable @$tableEl, @dataTableOpts
        @setObjs @objURL

    setObjs: (objURL) ->
      objURL ?= @objURL
      unless objURL is ""
        $.getJSON objURL, (objs) =>
          @dataTable.fnClearTable()
          rows = @objsToRows? objs
          @dataTable.fnAddData rows
      @

    setObjsToRowsConverter: (fun) ->
      @objsToRows = fun
      @

    setDataTableOptions: (options) ->
      @dataTableOpts = options
      @

    on: (eventName, elementName, callback) ->
      $("##{@tableName}-table").on(eventName, elementName, callback)
      @

  TableMan =
    Table: Table

