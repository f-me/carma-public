this.setupSupervisorScreen = (viewName, args) ->
  setTimeout ->
    $.fn.dataTableExt.oStdClasses.sLength = "dataTables_length form-inline"
    $.fn.dataTableExt.oStdClasses.sFilter = "dataTables_filter form-inline"

    t = $("#supervisor-table");
    return if t.hasClass("dataTable")
    dt = mkDataTable t,
      bPaginate: true
      fnRowCallback: (nRow, aData, iDisplayIndex, iDisplayIndexFull) ->
        caseId = aData[0].split('/')[0]
        caseLnk = "<a href='/#case/#{caseId}'> #{aData[0]} </a>"
        cdate = Date.parse aData[5]
        gdate = (new Date).setMinutes((new Date).getMinutes() + 30)
        now = new Date

        $('td:eq(0)', nRow).html caseLnk

        # if duedate is past, make it red
        if cdate < now
          $(nRow).children().css('background-color', '#ff5555')
        # if duedate will be past in 30 sec, make it green
        else if cdate < gdate
          $(nRow).children().css('background-color', '#99ff00')

    $('#reload').click -> dtRedraw(dt)

    # deep copy
    r = $.extend(true, {}, global.dictionaries.Roles)
    r.entries.unshift {value: "", label: "Все роли"}

    ko.applyBindings r, $('#role')[0]
    t.on "click.datatable", "tr", ->
      id = this.children[0].innerText.split('/')[1].replace(/\D/g,'')
      f = ["assignedTo", "priority", "closed", "targetGroup"]
      modelSetup("action") viewName, {"id": id},
                            permEl: "action-permissions"
                            focusClass: "focusable"
                            refs: []
                            forceRender: f

      knockVM = global.viewsWare['action-form'].knockVM

    d1 = (new Date).addDays(-14)
    d2 = (new Date).addDays(+7)
    $('#date-min').val d1.toString('dd.MM.yyyy HH:mm')
    $('#date-max').val d2.toString('dd.MM.yyyy HH:mm')
    dtRedraw dt

drawTable = (dt, opt) ->
  select = []
  select.push("closed=#{opt.closed}") if opt.closed
  select.push("targetGroup=#{opt.targetGroup}") if opt.targetGroup
  select.push("duetimeFrom=#{opt.duetimeFrom}") if opt.duetimeFrom
  select.push("duetimeTo=#{opt.duetimeTo}") if opt.duetimeTo
  $.getJSON "/allActions?#{select.join('&')}",
      (objs) ->
          dt.fnClearTable()

          n = global.dictValueCache['ActionNames']
          r = global.dictValueCache['ActionResults']
          u = global.dictValueCache['users']
          g = global.dictValueCache['Roles']

          rows = for obj in objs
            svcName = obj.parentId.split(':')[0]
            svcName = global.models[svcName].title
            cid = obj.caseId.split(':')[1]
            closed = if obj.closed == "1"
                'Закрыто'
               else
                 'Открыто'
            duetime = new Date(obj.duetime * 1000)
              .toString("dd.MM.yyyy HH:mm:ss")
            [ "#{cid}/#{obj.id} (#{svcName})"
            , closed
            , n[obj.name] || ''
            , u[obj.assignedTo] || ''
            , g[obj.targetGroup] || obj.targetGroup || ''
            , duetime || ''
            , r[obj.result]  || ''
            , obj.priority || ''
            ]
          dt.fnAddData(rows)
          dt.fnSort [[5,'asc']]
          $('select[name=supervisor-table_length]').val(100)
          $('select[name=supervisor-table_length]').change()

dtRedraw = (dt) ->
  d1 = Date.parse $('#date-min').val()
  d2 = Date.parse $('#date-max').val()
  return unless d1 and d2
  drawTable dt,
    closed: $('#closed').val()
    targetGroup: $('#role').val()
    duetimeFrom: toUnix d1
    duetimeTo: toUnix d2
