define "screenman", ["utils", "model/main"], (utils, main) ->
  
  class Table
    constructor: (params) ->
      {@tableName, @objURL} = params
      @sLength = "dataTables_length form-inline"
      @sFilter = "dataTables_filter form-inline"
      @columns = []
      
    show: ->
      $.fn.dataTableExt.oStdClasses.sLength = @sLength
      $.fn.dataTableExt.oStdClasses.sFilter = @sFilter
      
      $table = $("##{@tableName}-table")
      unless $table.hasClass "dataTable"
        utils.mkDataTable $table
        $.getJSON @objURL, (objs) ->
          dataTable = $table.dataTable()
          dataTable.fnClearTable()
          rows = (makeRow obj for obj in objs)
          dataTable.fnAddData rows
          
        makeRow = (obj) =>
          formatRow = (columns) ->
            _.map columns, (column) ->
              switch column
                when "id" then obj[column].split(':')[1]
                else obj[column] || ""
          
          if _.isEmpty @columns
            columns = _.keys obj
            formatRow columns
          else
            formatRow @columns
          
    
    setColumns: (columns) ->
      @columns = columns
      @
    
    on: (eventName, elementName, callback) ->
      $("##{@tableName}-table").on(eventName, elementName, callback)
      @
  
  class Screen
    constructor: (params) ->
      {@modelName, @tpl, @viewName} = params
      @permEl = "#{@modelName}-permissions"
      @focusClass = "focusable"
      @slotSee = []
      @refs = []
      @groupsForest = ""
      @table = null
      
    show: (args) ->
      options = {@permEl, @focusClass, @slotSee, @refs, @groupForest}
      kvm = main.modelSetup(@modelName) @viewName, args, options
      do @table?.show
      
    addTable: (params) ->
      @table = new Table(params)

  class ScreenMan
    # screens array
    # key -> screen's name
    # value -> instance of the Screen class
    screens = []

    addScreen: (name, params) ->
      screens[name] = new Screen(params)

    showScreen: (name, args) ->
      screens[name]?.show args

  new ScreenMan