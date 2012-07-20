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
  fields = "id,caller_name,callDate,caller_phone1,car_plateNum,car_vin,program,comment"
  $.getJSON("/all/case?orderby=callDate&limit=70&fields=#{fields}", (objs) ->
    st.fnClearTable()
    dict = global.dictValueCache
    for i of objs
      obj = objs[i]
      continue if obj.id.length > 10
      plateNum = if obj.car_plateNum?
              obj.car_plateNum.toUpperCase()
          else
              ""
      carVin = if obj.car_vin?
              obj.car_vin.toUpperCase()
          else
              ""
      row = [obj.id.split(":")[1]
            ,obj.caller_name || ''
            ,new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm:ss")
            ,obj.caller_phone1 || ''
            ,plateNum, carVin
            ,dict.Programs[obj.program] || obj.program || ''
            ,dict.Wazzup[obj.comment] || obj.comment || ''
            ]
      st.fnAddData(row)
  )
  setupHotkeys()
