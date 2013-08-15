define [ "utils"
       , "hotkeys"
       , "model/main"
       , "screens/partnersSearch"
       , "text!tpl/screens/call.html"
       ], (utils, hotkeys, main, pSearch, tpl) ->

  utils.build_global_fn 'makeCase', ['screens/newCase']
  utils.build_global_fn 'reloadScreen', ['utils']

  setupCallForm = (viewName, args) ->
    knockVM = main.modelSetup("call") viewName, args,
                       permEl     : "case-permissions"
                       slotsee    : ["call-number"]
                       focusClass : "focusable"
                       groupsForest : "center"
    $('input[name="callDate"]').parents('.control-group').hide()
    $('input[name="callTaker"]').parents('.control-group').hide()
    searchTable = $("#call-scrn-searchtable")
    st = utils.mkDataTable searchTable,
      bFilter : false
      fnRowCallback: (nRow) -> $($(nRow).children()[1]).addClass("capitalize")
    searchTable.on("click.datatable", "tr", ->
      if (searchTable.fnGetPosition this) != null
        id = this.children[0].innerText
        window.location.hash = "case/" + id
    )

    $('#search-help').popover
      content: "Справка по поиску"

    sq = $('#search-query')
    sq.tagautocomplete
      character: '!'
      source:    {entries: ['!Кейс:', '!VIN:', '!Госномер:', '!Тел:']}

    e = jQuery.Event 'keypress'
    e.which = 61
    sq.keypress(_.debounce((-> dtSearch st), 1500))
      .change(-> sq.trigger e)
      .focus(-> sq.trigger e)

    st.fnSort [[2, "desc"]]
    dtSearch st
    hotkeys.setup()
    $("#search-partner").on 'click', partnerSearchClick

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
    q = $('#search-query').val().trim()
    q = q.replace '+', ''
    url = if q.length == 0 then "/latestCases" else "/searchCases?q=#{q}"
    $.getJSON url, (objs) -> fillTable st, objs

  partnerSearchClick = ->
    kvm = global.viewsWare['call-form'].knockVM
    if kvm.callerType() == "client" or _.isEmpty kvm.callerType()
      kvm.callerType("client")
      kvm.callType("switchDealer")
    localStorage[pSearch.storeKey] = JSON.stringify kvm._meta.q.toRawObj()
    pSearch.open('call')

  { constructor: setupCallForm
  , template: tpl
  }
