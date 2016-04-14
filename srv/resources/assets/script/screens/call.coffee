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
    batchSet = (obs, fields, fn) ->
      _.map fields, (f) -> knockVM[f]?[obs] (def, realNot) -> fn(f, realNot)

    if knockVM['callReason']
      knockVM['abuseTarget']?.customVisible ->
        _.contains complaints, knockVM['callReason']()

      knockVM['abuseTarget']?.customRequired (def, realNot) ->
        (_.contains complaints, knockVM['callReason']()) and realNot

      knockVM['customerComment']?.customRequired ->
        ((_.contains others, knockVM['callReason']()) or
        (knockVM.partner.text()?.toLowerCase() == "дилер не найден")) and
        not knockVM['customerComment']()

      required =
        ['program', 'callerName', 'callerPhone', 'callerType', 'callReason']
      batchSet 'customRequired', required, (f, realNot) ->
        knockVM['callType']() == callTypes['info'] and realNot

      knockVM['partner']?.customVisible ->
        knockVM['callReason']() == reasons["client_contactDealer"]

      knockVM['partner']?.customRequired ->
        knockVM['callReason']() == reasons["client_contactDealer"] and
        not _.isNumber knockVM['partner']()
      knockVM['abandonedServices'] = ko.observable(global.Usermeta?.abandonedServices() || [])
      global.Usermeta?.abandonedServices.subscribe (svcs) -> knockVM.abandonedServices(svcs)



    window.k = knockVM
    makeNewCase = ->
      knockVM['callType'](callTypes['newCase'])
      v = knockVM

      args =
        contact_name:         v['callerName']?()
        contact_phone1:       v['callerPhone']?()
        program:              v['program']()
        caseAddress_coords:   v['coords']?()
        caseAddress_address:  v['address']?()
        customerComment:      v['customerComment']?()

      main.buildNewModel 'Case', args, {modelArg: "ctr:#{v.program()}"},
        (m, k) ->
          v['caseId']?(k.id())
          Finch.navigate "case/#{k.id()}"

    btnsCtx =
      makeNewCase:
        fn:    _.throttle makeNewCase, 2000, {trailing: false}
        avail: ko.computed -> knockVM['program']()
      openDip:
        fn:  ->
          knockVM['callType'](callTypes['info'])
          knockVM['callerType'](callerTypes['client'])
          knockVM['callReason'](reasons['client_contactDealer'])
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

          knockVM._meta.q.save ->
            # check if we have id in url, then goto call; else just reload
            if location.hash.match(/[0-9]+$/)
            then Finch.navigate 'back'
            else reloadScreen()
        title:
          if knockVM.abandonedServices().length == 0
            ''
          else
            'Невозможно завершить звонок, у Вас есть услуга в статусе “Создание”'
        avail: ko.computed ->
          knockVM.abandonedServices().length == 0 and
            _.all _.map knockVM._meta.model.fields, (f) ->
              ! knockVM["#{f.name}Not"]()

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

  { constructor: setupCallForm
  , template: tpl
  }
