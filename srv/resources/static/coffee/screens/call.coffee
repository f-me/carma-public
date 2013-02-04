define [], ->
  setupCallForm = (viewName, args) ->
    knockVM = modelSetup("call") viewName, args,
                       permEl     : "case-permissions"
                       slotsee    : ["call-number"]
                       focusClass : "focusable"
                       groupsForest : "center"
    knockVM['callTaker'](global.user.meta.realName)
    $('input[name="callDate"]').parents('.control-group').hide()
    $('input[name="callTaker"]').parents('.control-group').hide()
    searchTable = $("#call-searchtable")
    st = mkDataTable searchTable,
      bFilter : false
      fnRowCallback: (nRow) -> $($(nRow).children()[1]).addClass("capitalize")
    searchTable.on("click.datatable", "tr", ->
      id = this.children[0].innerText
      window.location.hash = "case/" + id
    )

    $('#search-query').keypress(_.debounce((-> dtSearch st), 1500))
    $('#search-query').change(-> dtSearch st)

    st.fnSort [[2, "desc"]]
    dtSearchQ st, new Date().toString("dd.MM.yyyy")
    setupHotkeys()

  fillTable = (st, objs) ->
    st.fnClearTable()
    dict = global.dictValueCache
    rows = for obj in objs
      continue if obj.id.length > 10
      row = [obj.id.split(":")[1] || obj.id
            ,obj.contact_name || ''
            ,new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm:ss")
            ,obj.contact_phone1 || ''
            ,(obj.car_plateNum || "").toUpperCase()
            ,(obj.car_vin || "").toUpperCase()
            ,dict.Programs[obj.program] || obj.program || ''
            ,dict.Wazzup[obj.comment] || obj.comment || ''
            ]
    st.fnAddData(rows)

  dtSearch = (st) ->
    dtSearchQ st, $('#search-query').val()

  dtSearchQ = (st, q) ->
    fields = "id,contact_name,callDate,contact_phone1,car_plateNum,car_vin,program,comment"
    searchIn = "id,callDate,comment,callTaker,betaComment,caseStatus,city,dealerCause,contact_name,contact_phone1,contact_phone2,contact_phone3,contact_phone4,contact_ownerEmail,contact_ownerName,contact_ownerPhone1,contact_ownerPhone2,contact_ownerPhone3,contact_ownerPhone4,car_vin,car_plateNum,car_make,car_model,car_makeYear,car_buyDate,car_color,car_checkupDate,car_seller,car_dealerTO,cardNumber_cardNumber,cardNumber_cardOwner,caseAddress_address,program"
    $.getJSON("/search/case?q=#{q}&fields=#{searchIn}&select=#{fields}&limit=120", (objs) ->
      fillTable(st, objs))

  { constructor: setupCallForm }