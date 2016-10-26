define ["utils"
      , "model/main"
      , "text!tpl/screens/supervisor.html"
      , "screenman"
      , "hooks/common"
      , "text!tpl/fields/form.html"
      ], (utils, main, tpl, screenman, hook, Flds) ->

  flds =  $('<div/>').append($(Flds))

  dataTableOptions = ->
    aoColumns: utils.repeat(11, null).concat utils.repeat(2, { bVisible: false})
    bPaginate: true
    fnRowCallback: (nRow, aData, iDisplayIndex, iDisplayIndexFull) ->
      return if _.isEmpty aData
      caseId = aData[0].split('/')[0]
      caseLnk =
        if caseId != '-'
          "<a style='color: black' href='/#case/#{caseId}'>#{aData[0]}</a>"
        else
          aData[0]
      duetime  = Date.parse aData[5]
      srvStart = Date.parse aData[11]
      mktime = (n) ->
        d = new Date
        d.setMinutes(d.getMinutes() + n)
        return d
      d60  = mktime 60
      d120 = mktime 120
      d480 = mktime 480
      now  = new Date
      name = aData[12]

      $('td:eq(0)', nRow).html caseLnk

      green  = "#99ff66"
      orange = "#ff6600"
      yellow = "#ffcc33"
      red    = "#ff6666"
      violet = "#9999ff"

      set = (clr) -> $(nRow).children().css('background-color', clr)

      time = if name == 'orderService' or name == 'orderServiceAnalyst'
               srvStart || duetime
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

  objsToRows = (res) ->
    ar = utils.newModelDict "ActionResult", true
    at = utils.newModelDict "ActionType", true
    cities = utils.newModelDict "City", true
    u = global.dictValueCache['users']

    roles = utils.newModelDict "Role", true
    progs = utils.newModelDict "Program", true
    svcTypes = utils.newModelDict "ServiceType", true

    rows = for obj in res.actions
      if obj.serviceId
        svcName = svcTypes.getLab obj.serviceType
        svcName = "(#{svcName})"
      else
        svcName = null
      cid = obj.caseId
      closed = !(_.isEmpty obj.result)
      closedLab = if closed
          'Закрыто'
         else
          'Открыто'
      duetime = new Date(obj.duetime * 1000)
        .toString("dd.MM.yyyy HH:mm:ss")
      srvStart = new Date(obj.times_expectedServiceStart * 1000)
        .toString("dd.MM.yyyy HH:mm:ss")
      timeLabel =
        if _.isEmpty obj.assignedTo
          utils.timeFrom obj.ctime, res.reqTime
        else
          if !closed
            utils.timeFrom obj.assignTime, res.reqTime
          else
            utils.timeFrom obj.openTime, obj.closeTime
      [ "#{cid or '-'}/#{obj.id} #{svcName or ''}"
      , closedLab
      , (at.getLab obj.name) || ''
      , u[obj.assignedTo] || ''
      , roles.getLab(obj.targetGroup) || obj.targetGroup || ''
      , duetime || ''
      , timeLabel?[0] || ''
      , (ar.getLab obj.result) || ''
      , obj.priority || ''
      , (cities.getLab obj.city) || ''
      , progs.getLab(obj.program) || ''
      , srvStart || ''
      , obj.name || ''
      ]

  formatObjURL = (roles) ->
    dateFrom = Date.parse $('#date-min').val()
    dateTo = Date.parse $('#date-max').val()
    if dateFrom and dateTo
      opt =
        closed: $('#closed').val()
        targetGroup: roles
        duetimeFrom: utils.toUnix dateFrom
        duetimeTo  : utils.toUnix dateTo

      select = []
      select.push("closed=#{opt.closed}") if opt.closed
      if opt.targetGroup and opt.targetGroup != "all"
        select.push("targetGroup=#{opt.targetGroup}")
      select.push("duetimeFrom=#{opt.duetimeFrom}") if opt.duetimeFrom
      select.push("duetimeTo=#{opt.duetimeTo}") if opt.duetimeTo
      objURL = "/backoffice/allActions?#{select.join('&')}"
    else
      ""

  modelSetup = (modelName, viewName, args) ->
    permEl = "#{modelName}-permissions"
    focusClass = "focusable"
    refs = []
    forceRender = ["assignedTo", "priority", "closed", "targetGroup"]
    manual_save = true
    options = {permEl, focusClass, refs, forceRender, manual_save}
    main.modelSetup(modelName) viewName, args, options

  roleFieldSetup = ->
    roleModel =
      fields: [
        name: "roles"
        type: "dictionary-many"
        meta:
          label: "Роли"
          dictionaryType: "ComputedDict"
          dictionaryName: "backofficeRoles"
          bounded: true
      ]

    roleKVM = main.buildKVM roleModel, {}
    hook.dictManyHook roleModel, roleKVM
    tpl = $(flds).find('#dictionary-many-field-template').html()
    $('#roles').html(Mustache.render tpl, roleModel.fields[0])
    ko.applyBindings roleKVM, $('#roles')[0]
    roleKVM.roles [ global.idents("Role").bo_order
                  , global.idents("Role").bo_orderRefs
                  , global.idents("Role").bo_orderAvarcom
                  , global.idents("Role").bo_controlRefs
                  , global.idents("Role").bo_controlAvarcom
                  , global.idents("Role").bo_info
                  , global.idents("Role").bo_secondary
                  , global.idents("Role").bo_urgent
                  ]
    roleKVM

  # Update unassigned action counts using currently selected duetime
  # limits
  updateActStats = () ->
    dateFrom = Date.parse $('#date-min').val()
    dateTo = Date.parse $('#date-max').val()
    select = []
    if dateFrom and dateTo
        duetimeFrom = utils.toUnix dateFrom
        select.push("duetimeFrom=#{duetimeFrom}") if duetimeFrom
        duetimeTo = utils.toUnix dateTo
        select.push("duetimeTo=#{duetimeTo}") if duetimeTo
    $.getJSON "/supervisor/actStats?#{select.join('&')}", (as) ->
      $("#unassigned-orders").text(as.order)
      $("#unassigned-controls").text(as.control)

  screenSetup = (viewName, args) ->
    dateFrom = (new Date).addDays(-1)
    dateTo = (new Date).addDays(+1)
    $('#date-min').val dateFrom.toString('dd.MM.yyyy HH:mm')
    $('#date-max').val dateTo.toString('dd.MM.yyyy HH:mm')

    roleKVM = roleFieldSetup()

    objURL = formatObjURL roleKVM.roles()
    tableParams =
      tableName: "supervisor"
      objURL: objURL

    modelName = "Action"

    table = screenman.addScreen(modelName, ->)
      .addTable(tableParams)
      .setObjsToRowsConverter(objsToRows)
      .setDataTableOptions(do dataTableOptions)
      .on("click.datatable", "tr", ->
        id = @children[0].innerText.split('/')[1].replace(/\D/g,'')
        modelSetup modelName, viewName, {id}
        global.viewsWare["action-form"].knockVM)

    screenman.showScreen modelName

    $('#reload').click ->
      objURL = formatObjURL roleKVM.roles()
      updateActStats()
      table.setObjs objURL unless objURL is ""


    table.dataTable.fnSort [[5,'asc']]
    $('select[name=supervisor-table_length]').val(100)
    $('select[name=supervisor-table_length]').change()

    updateActStats()

  { constructor: screenSetup
  , template: tpl
  }
