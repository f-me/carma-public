define ["utils", "model/main", "text!tpl/screens/supervisor.html", "screenman"], (utils, main, tpl, screenman) ->

  dataTableOptions = ->
    aoColumns: utils
      .repeat(10, null)
      .concat(utils.repeat(2, { bVisible: false}))
    bPaginate: true
    fnRowCallback: (nRow, aData, iDisplayIndex, iDisplayIndexFull) ->
      caseId = aData[0].split('/')[0]
      caseLnk = "<a style='color: black' href='/#case/#{caseId}'> #{aData[0]} </a>"
      duetime  = Date.parse aData[5]
      srvStart = Date.parse aData[10]
      mktime = (n) ->
        d = new Date
        d.setMinutes(d.getMinutes() + n)
        return d
      d60  = mktime 60
      d120 = mktime 120
      d480 = mktime 480
      now  = new Date
      name = aData[11]

      $('td:eq(0)', nRow).html caseLnk

      green  = "#99ff66"
      orange = "#ff6600"
      yellow = "#ffcc33"
      red    = "#ff6666"
      violet = "#9999ff"

      set = (clr) -> $(nRow).children().css('background-color', clr)

      time = if name == 'orderService' or name == 'orderServiceAnalyst'
               srvStart
             else
               duetime

      if time > d480
        set green
      if d120 < time < d480
        set orange
      if d60 < time < d120
        set yellow
      if now < time < d60
        set red
      if time < now
        set violet

  objsToRows = (objs) ->
    n = global.dictValueCache['ActionNames']
    r = global.dictValueCache['ActionResults']
    u = global.dictValueCache['users']
    g = global.dictValueCache['Roles']

    rows = for obj in objs
      if obj.parentId
        svcName = obj.parentId.split(':')[0]
        svcName = global.models[svcName].title
      cid = obj.caseId.split(':')[1]
      closed = if obj.closed == "1"
          'Закрыто'
         else
          'Открыто'
      duetime = new Date(obj.duetime * 1000)
        .toString("dd.MM.yyyy HH:mm:ss")
      srvStart = new Date(obj.times_expectedServiceStart * 1000)
        .toString("dd.MM.yyyy HH:mm:ss")
      [ "#{cid}/#{obj.id} (#{svcName or ''})"
      , closed
      , n[obj.name] || ''
      , u[obj.assignedTo] || ''
      , g[obj.targetGroup] || obj.targetGroup || ''
      , duetime || ''
      , r[obj.result] || ''
      , obj.priority || ''
      , global.dictValueCache['DealerCities'][obj.city] || ''
      , global.dictValueCache['Programs'][obj.program] || ''
      , srvStart || ''
      , obj.name || ''
      ]

  formatObjURL = ->
    dateFrom = Date.parse $('#date-min').val()
    dateTo = Date.parse $('#date-max').val()
    if dateFrom and dateTo
      opt =
        closed: $('#closed').val()
        targetGroup: $('#role').val()
        duetimeFrom: utils.toUnix dateFrom
        duetimeTo  : utils.toUnix dateTo

      select = []
      select.push("closed=#{opt.closed}") if opt.closed
      select.push("targetGroup=#{opt.targetGroup}") if opt.targetGroup
      select.push("duetimeFrom=#{opt.duetimeFrom}") if opt.duetimeFrom
      select.push("duetimeTo=#{opt.duetimeTo}") if opt.duetimeTo
      objURL = "/allActions?#{select.join('&')}"
    else
      ""

  modelSetup = (modelName, viewName, args) ->
    permEl = "#{modelName}-permissions"
    focusClass = "focusable"
    refs = []
    forceRender = ["assignedTo", "priority", "closed", "targetGroup"]
    options = {permEl, focusClass, refs, forceRender}
    main.modelSetup(modelName) viewName, args, options

  screenSetup = (viewName, args) ->
    dateFrom = (new Date).addDays(-14)
    dateTo = (new Date).addDays(+7)
    $('#date-min').val dateFrom.toString('dd.MM.yyyy HH:mm')
    $('#date-max').val dateTo.toString('dd.MM.yyyy HH:mm')

    # deep copy
    r = $.extend(true, {}, global.dictionaries.Roles)
    r.entries.unshift {value: "", label: "Все роли"}
    ko.applyBindings r, $('#role')[0]
    $('#role').val 'back'


    objURL = do formatObjURL
    tableParams =
      tableName: "supervisor"
      objURL: objURL

    modelName = "action"

    table = screenman.addScreen(modelName, ->)
      .addTable(tableParams)
      .setObjsToRowsConverter(objsToRows)
      .setDataTableOptions(do dataTableOptions)
      .on("click.datatable", "tr", ->
        id = @children[0].innerText.split('/')[1].replace(/\D/g,'')
        modelSetup modelName, viewName, {id}
        global.viewsWare["#{modelName}-form"].knockVM)

    screenman.showScreen modelName

    $('#reload').click ->
      objURL = do formatObjURL
      table.setObjs objURL unless objURL is ""


    table.dataTable.fnSort [[5,'asc']]
    $('select[name=supervisor-table_length]').val(100)
    $('select[name=supervisor-table_length]').change()

    $(->
      objURL = do formatObjURL
      table.setObjs objURL unless objURL is "")

  { constructor: screenSetup
  , template: tpl
  }
