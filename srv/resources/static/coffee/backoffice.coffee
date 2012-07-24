this.setupBackOffice = ->
  setTimeout((->
      tables = mkBoTable()
      global.boData = { started: new Date, r: {} }
      global.boData.iHandler =
        setInterval((-> $.getJSON("/ix/actionsForUser", setupBoTable)), 7000)
      $.getJSON("/ix/actionsForUser", setupBoTable)
      # non polling version for debug purposes
      # $.getJSON("/all/action", setupBoTable tables)
    ), 200)

this.removeBackOffice = ->
  h = global.boData.iHandler
  clearInterval h if h?

mkBoTable = ->
  groupTable = $("#back-group-table")
  gt = mkDataTable(groupTable)
  gt.fnSort [[2, "desc"]]
  groupTable.on("click.datatable", "tr", ->
    id = this.children[0].innerText.split('/');
    $.ajax
        type        : "PUT"
        url         : "/_/action/"+ id[1]
        contentType : "application/json"
        data        : '{"assignedTo":"'+global.user.login+'"}'
        processData : false
    window.location.hash = "case/" + id[0])
  userTable = $("#back-user-table")
  ut = mkDataTable(userTable)
  ut.fnSort [[2, "desc"]]
  userTable.on("click.datatable", "tr", ->
     id = this.children[0].innerText.split('/')
     window.location.hash = "case/" + id[0]
  )
  return [userTable, groupTable]

setupBoTable = (actions) ->
    userTable = $("#back-user-table")
    groupTable = $("#back-group-table")
    addActions(actions.user,  userTable.dataTable())
    addActions(actions.group, groupTable.dataTable())
    handleBoUpdate(groupTable)
    boNotify handleBoUpdate(userTable)



addActions = (actions, table) ->
  table.fnClearTable()
  for i of actions
    act = actions[i]
    continue if not act.caseId
    continue if not act.id
    continue if not act.duetime

    id = act.caseId.replace(/\D/g,'') + "/" + act.id.replace(/\D/g,'')
    duetime = new Date(act.duetime * 1000).toString("dd.MM.yyyy HH:mm:ss")
    row = [id
          ,act.priority || '3'
          ,duetime
          ,act.description || ''
          ,act.comment || '']
    table.fnAddData(row)


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
    $(e).children().css('background-color', '#ff6060') if now > date
    # last check to not notify about rows, that expired before
    # we open page
    if now > date and not row[id].checked and row[id].date > started
      toNotify.push e.id
      row[id].checked = true
  return toNotify

boNotify = (elems) ->
  alert "Измененные строки: #{elems.join(', ')}" unless _.isEmpty elems
