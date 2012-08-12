this.setupSupervisorScreen = (viewName, args) ->
  setTimeout ->
    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

    t = $("#supervisor-table");
    return if t.hasClass("dataTable")
    dt = mkDataTable(t, {bPaginate: true})

    $('#date-min').change -> dtRedraw(dt)
    $('#date-max').change -> dtRedraw(dt)

    t.on "click.datatable", "tr", ->
      id = this.children[0].innerText.split('/')[1]
      f = ["assignedTo", "priority", "closed", "targetGroup"]
      modelSetup("action") viewName, {"id": id},
                            permEl: "action-permissions"
                            focusClass: "focusable"
                            refs: []
                            forceRender: f
    d1 = (new Date).addDays(-2)
    d2 = (new Date).addDays(+7)
    drawTable dt, sb(d1, d2)

drawTable = (dt, select) ->
  fields = "id,caseId,closed,name,assignedTo,targetGroup
,duetime,result,priority"
  $.getJSON "/all/action?select=#{select}&fields=#{fields}",
      (objs) ->
          dt.fnClearTable()

          n = global.dictValueCache['ActionNames']
          r = global.dictValueCache['ActionResults']
          u = global.dictValueCache['users']

          rows = for obj in objs
            sid = obj.id.split(':')[1]
            cid = obj.caseId.split(':')[1]
            closed = if obj.closed then 'Закрыто' else 'Открыто'
            duetime = new Date(obj.duetime * 1000)
              .toString("dd.MM.yyyy HH:mm:ss")
            [ "#{cid}/#{sid}"
            , closed
            , n[obj.name] || ''
            , u[obj.assignedTo] || ''
            , obj.targetGroup
            , duetime || ''
            , r[obj.result]  || ''
            , obj.priority || ''
            ]
          dt.fnAddData(rows)

dtRedraw = (dt) ->
  d1 = Date.parse $('#date-min').val()
  d2 = Date.parse $('#date-max').val()
  return unless d1 and d2
  drawTable dt, sb(d1, d2)

sb = (d1,d2) -> "duetime >= #{toUnix d1}, duetime <= #{toUnix d2}"
