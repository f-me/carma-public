define [ "utils"
       , "lib/ident/role"
       , "text!tpl/screens/back.html"], (utils, role, tpl) ->
  onBackofficeScreen = true

  setupBackOffice = ->
    onBackofficeScreen = true
    setTimeout((->
        tables = mkBoTable()
        global.boData = { started: new Date, r: {} }
        params = "assignedTo=#{global.user.login}&closed=0"
        # FIXME: remove this, when backend will be fast enough (it
        # will, be sure)
        setTimeout (-> $.getJSON("/backoffice/allActions?#{params}",
                    (r) -> myActionsHandler r.actions)),
                    1500
        setupPoller()
      ), 200)

  # Install automatic actions poller
  setupPoller = ->
    # In ms
    cycle_resolution = 500
    # Poll server every n cycles
    poll_cycles = 30

    current_cycle = 0
    bar = $("#bo-wait-progress")
    worker = ->
      percent = current_cycle / poll_cycles * 100.0
      bar.css "width", percent + "%"

      if onBackofficeScreen
        if current_cycle++ == poll_cycles
          pullActions()
          current_cycle = 0
        setTimeout worker, cycle_resolution
    worker()

  # Given /allActions or /littleMoreActions response, try to redirect
  # to the first order-class action if the user has "bo_order" role.
  # Otherwise, just show all actions in the table.
  myActionsHandler = (actions) ->
    if !_.isEmpty actions
      if _.contains global.user.roles, role.bo_order
        act = _.find actions, (a) ->
          _.contains(
            [ "orderService"
            , "orderServiceAnalyst"
            , "tellMeMore"
            , "callMeMaybe"],
            a.name)
        if act?
          openCaseAction act.id, act.caseId.split(':')[1]
      setupBoTable actions

  # Pull new actions for user
  pullActions = ->
    $.ajax
      type: "PUT"
      url: "/backoffice/littleMoreActions"
      success: myActionsHandler

  removeBackOffice = ->
    # Stop auto-polling backoffice-related server handlers when we
    # leave #back
    onBackofficeScreen = false

  mkBoTable = ->
    userTable = $("#back-user-table")
    ut = utils.mkDataTable userTable,
      aoColumns: utils.repeat(6, null).concat(utils.repeat(2, { bVisible: false}))
      fnRowCallback: (nRow, aData, iDisplayIndex, iDisplayIndexFull) ->
        duetime  = Date.parse aData[2]
        srvStart = Date.parse aData[6]
        name     = aData[7]
        mktime = (n) ->
          d = new Date
          d.setMinutes(d.getMinutes() + n)
          return d
        d60  = mktime 60
        d120 = mktime 120
        d480 = mktime 480
        now  = new Date

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


    ut.fnSort [[2, "asc"]]
    userTable.on("click.datatable", "tr", ->
      colText = this.children[0].innerText
      [_,caseId,actId] = colText.match(/(\d+)\/(\d+)/)
      openCaseAction actId, caseId
    )
    return [userTable]

  # Start working on an action and redirect to its case
  openCaseAction = (actId, caseId) ->
    $.ajax
      type: "PUT"
      url: "/backoffice/openAction/#{actId}"
    window.location.hash = "case/#{caseId}"

  setupBoTable = (actions) ->
      userTable = $("#back-user-table")
      addActions(actions,  userTable.dataTable())
      boNotify handleBoUpdate(userTable)

  addActions = (actions, table) ->
    table.fnClearTable()
    rows = for act in actions when act.caseId
      cid = act.caseId.split(':')[1]
      if act.parentId
        svcName = act.parentId.split(':')[0]
        svcName = global.model(svcName).title
      id = "#{cid}/#{act.id} (#{svcName or ''})"
      duetime  = new Date(act.duetime * 1000).toString("dd.MM.yyyy HH:mm:ss")
      srvStart = new Date(act.times_expectedServiceStart * 1000)
                   .toString("dd.MM.yyyy HH:mm:ss")
      row = [ id
            , act.priority || '3'
            , duetime
            , global.dictValueCache['DealerCities'][act.city] || ''
            , act.description || ''
            , act.comment || ''
            , srvStart || ''
            , act.name || ''
            ]
    table.fnAddData(rows)


  # mark expired entries and notify with alert of just expired rows
  handleBoUpdate = (table) ->
    toNotify = []
    table.find('.odd, .even').each (i,e) ->
      row     = global.boData.r
      started = global.boData.started
      [id, _, d] = $(e).children().map (i,e)-> $(e).text()
      date = new Date.parse(d)
      now  = new Date
      $(e).attr('id', id)
      row[id] = { date: date } unless row[id]?
      # $(e).children().css('background-color', '#FFF855') if now > date
      # last check to not notify about rows, that expired before
      # we open page
      if now > date and not row[id].checked and row[id].date > started
        toNotify.push e.id
        row[id].checked = true
    return toNotify

  boNotify = (elems) ->
    alert "Ой! Действия просрочились: #{elems.join(', ')}" unless _.isEmpty elems

  { constructor: setupBackOffice
  , destructor: removeBackOffice
  , template: tpl
  }
