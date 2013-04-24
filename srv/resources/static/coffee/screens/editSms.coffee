define ["model/main", "text!tpl/screens/editSms.html", "screenman"],
  (main, tpl, screenman) ->
    setupForm = (viewName, args) ->
      screenParams =
        modelName: "smsTpl"
        tpl: tpl
        viewName: viewName
      tableParams = 
        tableName: "sms"
        objURL: "/all/smsTpl?fields=id,name,text,notActive"
      screenman.addScreen("smsTpl", screenParams)
        .addTable(tableParams)
        .setColumns(["id", "name", "text"])
        .on("click.datatable", "tr", ->
          id = this.children[0].innerText
          main.modelSetup("smsTpl") viewName, {"id": id},
                                permEl: "smsTpl-permissions"
                                focusClass: "focusable"
                                refs: []
          )
      screenman.showScreen "smsTpl", args
    
    constructor: setupForm
    template   : tpl
