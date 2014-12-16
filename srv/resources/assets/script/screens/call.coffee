define [ "utils"
       , "hotkeys"
       , "model/main"
       , "screens/partnersSearch"
       , "text!tpl/screens/call.html"
       , "lib/messenger"
       ], (utils, hotkeys, main, pSearch, tpl, Msg) ->

  utils.build_global_fn 'reloadScreen', ['utils']
  storeKey = "call"

  setupCallForm = (viewName, args) ->

    # change z-index, so menu wil be shown even with active modal
    $("#new-call-modal").on "shown.bs.modal", ->
      $(".modal-backdrop").css "z-index", 1029
    $("#new-call-modal").on "hide.bs.modal", ->
      $(".modal-backdrop").css "z-index", "1040"

    $("#make-new-call").on 'click', -> makeCallClick viewName

    # if user have unfinished call redirect him to close it
    unfinished = localStorage["#{storeKey}.id"]
    if unfinished and args.id isnt unfinished
        return Finch.navigate "call/#{unfinished}"

    knockVM = main.modelSetup("Call") viewName, args,
                       permEl      : "case-permissions"
                       slotsee     : ["call-number", "center"]
                       focusClass  : "focusable"
                       groupsForest: "center"

    callTypes = window.global.idents('CallType')
    callerTypes = window.global.idents('CallerType')
    reasons = window.global.idents('CallReason')
    complaints = _.compact [ reasons.client_complaint
                           , reasons.partner_complaint
                           , reasons.dealer_complaint
                           ]

    others = _.compact [ reasons.client_other
                       , reasons.partner_other
                       , reasons.dealer_other
                       , reasons.employee_other
                       , reasons.other_other
                       ]

    if knockVM['callReason']
      knockVM['abuseTarget']?.customVisible ->
        _.contains complaints, knockVM['callReason']()

      knockVM['customerComment']?.customRequired ->
        _.contains others, knockVM['callReason']()

      knockVM['partner']?.customVisible ->
        knockVM['callReason']() == reasons["client_contactDealer"]

    window.k = knockVM
    makeNewCase = ->
      knockVM['callType'](callTypes['newCase'])
      v = knockVM

      args =
        contact_name:         v['callerName']?()
        contact_phone1:       v['callerPhone']?()
        program:              v['program']()
        caseAddress_coords:   v['coords']()
        caseAddress_address:  v['address']()
        customerComment:      v['customerComment']?()

      main.buildNewModel 'Case', args, {modelArg: "ctr:#{v.program()}"},
        (m, k) ->
          v['caseId']?(k.id())
          Finch.navigate "case/#{k.id()}"

    btnsCtx =
      makeNewCase:
        fn:    _.throttle makeNewCase, 2000, {trailing: false}
        avail: ko.computed -> knockVM['program']()?

      openDip:
        fn:  ->
          knockVM['callType'](callTypes['info'])
          knockVM['callerType'](callerTypes['client'])
          # Subscribe call model to updates to coords & address fields
          for f in ["coords", "address", "partner"]
            do (f) ->
              n = pSearch.subName f, "call", knockVM.id()
              global.pubSub.sub n, knockVM[f]

          localStorage[pSearch.storeKey] =
            JSON.stringify knockVM._meta.q.toRawObj()
          pSearch.open('call')

      endCall:
        fn: ->
          if _.isNull knockVM.endDate()
            knockVM.endDate(new Date().toString("dd.MM.yyyy HH:mm:ss"))

          localStorage.removeItem "#{storeKey}.id"

          knockVM._meta.q.save ->
            # check if we have id in url, then goto call; else just reload
            if location.hash.match(/[0-9]+$/)
            then Finch.navigate 'call'
            else reloadScreen()
        avail: ko.computed ->
          (knockVM['callReason']() == reasons['client_contactDealer']) and
          (_.isNumber knockVM['partner']())

      servicesSearch:
        fn: ->
          knockVM.callType(callTypes['secondCall'])
          Finch.navigate 'search/services'

      callsSearch:
        fn: -> Finch.navigate 'search/calls'

      contractsSearch:
        fn: ->
          knockVM.callType(callTypes['info'])
          knockVM['callerType'](callerTypes['client'])
          Finch.navigate 'search/contracts'

    ko.applyBindings(btnsCtx, $("#right")[0])

    searchTable = $("#call-scrn-searchtable")
    st = utils.mkDataTable searchTable,
      bFilter : false
      fnRowCallback: (nRow) -> $($(nRow).children()[1]).addClass("capitalize")
    searchTable.on("click.datatable", "tr", ->
      if (searchTable.fnGetPosition this) != null
        knockVM.callType(callTypes['secondCall'])
        id = this.children[0].innerText
        window.location.hash = "case/" + id
    )

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

    # this will prevent modal from hiding on click behind modal borders
    $("#new-call-modal").modal { backdrop: 'static', show: false }
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

  makeCallClick = (viewName) ->
    cb = ->
      hideModal()
      kvm = global.viewsWare[viewName].knockVM
      localStorage["#{storeKey}.id"] = kvm.id()
    saveInstance viewName, cb, true


  setModalVisible = (visible) ->
    if visible then showModal() else hideModal()

  showModal = ->
    $("#new-call-modal").show().removeClass("out").addClass("in")
    $("#call-screen").css('visibility', 'hidden')

  hideModal = ->
    $("#new-call-modal").hide().removeClass("in").addClass("out")
    $("#call-screen").css('visibility', 'visible')


  { constructor: setupCallForm
  , template: tpl
  }
