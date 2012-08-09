this.setupCallForm = (viewName, args) ->
  knockVM = modelSetup("call") viewName, args,
                     permEl     : "case-permissions"
                     focusClass : "focusable"
                     groupsForest : "center"
  knockVM['callTaker'](global.user.meta.realName)
  $('input[name="callDate"]').parents('.control-group').hide()
  $('input[name="callTaker"]').parents('.control-group').hide()
  searchTable = $("#call-searchtable")
  st = mkDataTable(searchTable)
  searchTable.on("click.datatable", "tr", ->
    id = this.children[0].innerText
    window.location.hash = "case/" + id
  )
  st.fnSort [[2, "desc"]]
  fields = "id,caller_name,contact_name,callDate,caller_phone1,contact_phone1,car_plateNum,car_vin,program,comment"
  $.getJSON("/all/case?orderby=callDate&limit=120&fields=#{fields}", (objs) ->
    st.fnClearTable()
    dict = global.dictValueCache
    rows = for obj in objs
      continue if obj.id.length > 10
      row = [obj.id.split(":")[1]
            ,obj.caller_name || obj.contact_name || ''
            ,new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm:ss")
            ,obj.caller_phone1 || obj.contact_phone1 || ''
            ,(obj.car_plateNum || "").toUpperCase()
            ,(obj.car_vin || "").toUpperCase()
            ,dict.Programs[obj.program] || obj.program || ''
            ,dict.Wazzup[obj.comment] || obj.comment || ''
            ]
    st.fnAddData(rows)
  )
  setupHotkeys()
