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
                            forceRender: ["assignedTo", "priority", "closed"]

    $.getJSON "/all/action?fields=id,caseId,closed,name,assignedTo,
duetime,result,priority",
        (objs) ->
            dt = t.dataTable()
            dt.fnClearTable()

            n = global.dictValueCache['ActionNames']
            r = global.dictValueCache['ActionResults']
            u = global.dictValueCache['users']
            o = global.dictValueCache['roles']

            rows = for obj in objs
              sid = obj.id.split(':')[1]
              cid = obj.caseId.split(':')[1]
              closed = if obj.closed then 'Y' else 'N'
              duetime = new Date(obj.duetime * 1000)
                .toString("dd.MM.yyyy HH:mm:ss")
              [ "#{cid}/#{sid}"
              , closed
              , n[obj.name] || ''
              , u[obj.assignedTo] || ''
              , o[obj.assignedTo]?.split(',')[0] || ''
              , duetime || ''
              , r[obj.result]  || ''
              , obj.priority || ''
              ]
            dt.fnAddData(rows)
