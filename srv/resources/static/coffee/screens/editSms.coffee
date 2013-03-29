define ["utils", "model/main", "text!tpl/screens/editSms.html"],
  (utils, main, tpl) ->
    setupSmsTplForm = (viewName, args) ->

      refs = [ ]
      main.modelSetup("smsTpl") viewName, args,
                            permEl: "smsTpl-permissions"
                            focusClass: "focusable"
                            refs: refs

      setTimeout(->
        $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
        $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

        t = $("#sms-table");
        return if t.hasClass("dataTable")
        utils.mkDataTable(t)

        t.on("click.datatable", "tr", ->
          id = this.children[0].innerText
          main.modelSetup("smsTpl") viewName, {"id": id},
                                permEl: "smsTpl-permissions"
                                focusClass: "focusable"
                                refs: refs
        )

        $.getJSON("/all/smsTpl?fields=id,name,text,notActive",
            (objs) ->
                dt = t.dataTable()
                dt.fnClearTable()
                rows = for obj in objs
                    [obj.id.split(':')[1]
                    ,obj.name || ''
                    ,obj.text || ''
                    ]
                dt.fnAddData(rows)
        ))
    constructor: setupSmsTplForm
    template   : tpl
