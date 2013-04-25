define "screenman", ["utils", "model/main"], (utils, main) ->
  
  class Table
    constructor: (params) ->
      {@tableName, @objURL} = params
      @sLength = "dataTables_length form-inline"
      @sFilter = "dataTables_filter form-inline"
      @objsToRows = null
      @dataTable = null
      @dataTableOpts = null
      
    show: ->
      $.fn.dataTableExt.oStdClasses.sLength = @sLength
      $.fn.dataTableExt.oStdClasses.sFilter = @sFilter
      
      $table = $("##{@tableName}-table")
      unless $table.hasClass "dataTable"
        @dataTable = utils.mkDataTable $table, @dataTableOpts
        @setObjs @objURL
    
    setObjs: (objURL) ->
      objURL ?= @objURL
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
  
  class Screen
    constructor: (@callback) ->
      @table = null
      
    show: ->
      do @callback
      do @table?.show
      
    addTable: (params) ->
      @table = new Table(params)
    
    getTable: ->
      @table

  class ScreenMan
    # screens array
    # key -> screen's name
    # value -> instance of the Screen class
    screens = []

    addScreen: (name, callback) ->
      screens[name] = new Screen(callback)

    showScreen: (name) ->
      do screens[name]?.show

  new ScreenMan