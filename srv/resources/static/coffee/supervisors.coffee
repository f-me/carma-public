this.setupSupervisorScreen = (viewName, args) ->
  setTimeout ->
    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

    t = $("#supervisor-table");
    return if t.hasClass("dataTable")
    mkDataTable(t)

    t.on "click.datatable", "tr", ->
      id = this.children[0].innerText.split('/')[1]
      console.log viewName
      modelSetup("action") viewName, {"id": id},
                            permEl: "action-permissions"
                            focusClass: "focusable"
                            refs: []

    $.getJSON "/all/action?fields=id,caseId,closed,name,assignedTo,
duetime,result,priority",
        (objs) ->
            dt = t.dataTable()
            dt.fnClearTable()

            n = global.dictValueCache['ActionNames']
            r = global.dictValueCache['ActionResults']

            rows = for obj in objs
              sid = obj.id.split(':')[1]
              cid = obj.caseId.split(':')[1]
              closed = if obj.closed then 'Y' else 'N'

              [ "#{cid}/#{sid}"
              , closed
              , n[obj.name] || ''
              , obj.assignedTo || ''
              , "back"
              , obj.duetime || ''
              , r[obj.result]  || ''
              , obj.priority || ''
              ]
            dt.fnAddData(rows)
