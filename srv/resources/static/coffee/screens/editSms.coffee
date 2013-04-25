define ["model/main", "text!tpl/screens/editSms.html", "screenman"],
  (main, tpl, screenman) ->
    modelSetup = (modelName, viewName, args) ->
      permEl = "#{modelName}-permissions"
      focusClass = "focusable"
      refs = []
      options = {permEl, focusClass, refs}
      main.modelSetup(modelName) viewName, args, options
    
    objsToRows = (objs) ->
      columns = ["id", "name", "text"]
      makeRow = (obj) ->
        _.map columns, (column) ->
          switch column
            when "id" then obj[column].split(':')[1]
            else obj[column] || ""
      
      rows = (makeRow obj for obj in objs)
    
    screenSetup = (viewName, args) ->
      modelName = "smsTpl"
      
      tableParams = 
        tableName: "sms"
        objURL: "/all/smsTpl?fields=id,name,text,notActive"
      
      screenman.addScreen(modelName, ->
        modelSetup modelName, viewName, args)
        .addTable(tableParams)
        .setObjsToRowsConverter(objsToRows)
        .on("click.datatable", "tr", ->
          id = @children[0].innerText
          modelSetup modelName, viewName, {id})
      
      screenman.showScreen modelName
    
    constructor: screenSetup
    template   : tpl
