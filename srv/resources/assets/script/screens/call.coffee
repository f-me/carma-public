define [ "utils"
       , "hotkeys"
       , "model/main"
       , "screens/partnersSearch"
       , "text!tpl/screens/call.html"
       ], (utils, hotkeys, main, pSearch, tpl) ->

  utils.build_global_fn 'makeCase', ['screens/case']
  utils.build_global_fn 'reloadScreen', ['utils']
  storeKey = "call"

  setupCallForm = (viewName, args) ->
    # if user have unfinished call redirect him to close it
    unfinished = localStorage["#{storeKey}.id"]
    if unfinished and args.id isnt unfinished
        return Finch.navigate "call/#{unfinished}"

    knockVM = main.modelSetup("Call") viewName, args,
                       permEl     : "case-permissions"
                       slotsee    : ["call-number", "right"]
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

    isProgramDefined = ->
      p = knockVM.program()
      p && p != ''
    $('#new-case').prop 'disabled', not isProgramDefined()
    knockVM.program.subscribe (pgm) ->
      $('#new-case').prop 'disabled', not isProgramDefined()

    $('#search-help').popover
      content: "Справка по поиску"

    sq = $('#search-query')
    # sq.tagautocomplete
    #   character: '!'
    #   source:    {entries: ['!Кейс:', '!VIN:', '!Госномер:', '!Тел:']}

    e = jQuery.Event 'keypress'
    e.which = 61
    sq.keypress(_.debounce((-> dtSearch st), 1500))
      .change(-> sq.trigger e)
      .focus(-> sq.trigger e)

    st.fnSort [[2, "desc"]]
    dtSearch st
    hotkeys.setup()
    $("#search-partner").on 'click', partnerSearchClick
    $("#make-new-call").on 'click', -> makeCallClick viewName
    $("#end-call").on 'click', -> endCallClick viewName
    setModalVisible not args.id?

    searchQuery = localStorage["#{storeKey}.search-query"]
    if searchQuery
      $("#search-query").val(searchQuery)
      $("#search-query").change()
      localStorage.removeItem "#{storeKey}.search-query"

  fillTable = (st, objs) ->
    st.fnClearTable()
    dict = global.dictValueCache
    progs = utils.newModelDict "Program", true
    wazzup = utils.newModelDict "Wazzup", true
    rows = for obj in objs
      continue if obj.id.length > 10
      row = [obj.id.split(":")[1] || obj.id
            ,obj.contact_name || ''
            ,new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm:ss")
            ,obj.contact_phone1 || ''
            ,(obj.car_plateNum || "").toUpperCase()
            ,(obj.car_vin || "").toUpperCase()
            ,progs.getLab(obj.program) || obj.program || ''
            ,wazzup.getLab(obj.comment) || obj.comment || ''
            ]
    return if _.isEmpty rows
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

    # Subscribe call model to updates to coords & address fields
    for f in ["coords", "address"]
      do (f) ->
        n = pSearch.subName f, "call", kvm.id()
        global.pubSub.sub n, kvm[f]

    localStorage[pSearch.storeKey] = JSON.stringify kvm._meta.q.toRawObj()
    pSearch.open('call')


  makeCallClick = (viewName) ->
    cb = ->
      hideModal()
      kvm = global.viewsWare[viewName].knockVM
      localStorage["#{storeKey}.id"] = kvm.id()
    saveInstance viewName, cb, true


  endCallClick = (viewName) ->
    kvm = global.viewsWare[viewName].knockVM
    if _.isNull kvm.endDate()
      kvm.endDate(new Date().toString("dd.MM.yyyy HH:mm:ss"))

    localStorage.removeItem "#{storeKey}.id"

    saveInstance viewName, ->
      # check if we have id in url, then goto call; else just reload
      if location.hash.match(/[0-9]+$/)
      then Finch.navigate 'call'
      else reloadScreen()

  setModalVisible = (visible) ->
    if visible then showModal() else hideModal()

  showModal = ->
    $("#new-call-modal").modal('show')
    $("#left").hide()
    $("#center").hide()
    $("#right").hide()
    $("#bottom").hide()

  hideModal = ->
    $("#left").show()
    $("#center").show()
    $("#right").show()
    $("#bottom").show()
    $("#new-call-modal").modal('hide')


  { constructor: setupCallForm
  , template: tpl
  }
