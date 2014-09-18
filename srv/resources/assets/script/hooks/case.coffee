define ["utils", "model/utils", "dictionaries"], (u, mu, d) ->
  fillEventsHistory = (knockVM) -> ->

    # Disable hooks on search screen #1985
    return if /^search/.test(Finch.navigate())

    t = $("#call-searchtable")
    st = t.dataTable()
    # return if table template is not yet rendered
    # FIXME: remove dat shit and some day, use more knockout friendly solution
    unless $("#call-searchtable")[0]
      setTimeout fillEventsHistory(knockVM), 300
      return

    phone = knockVM['contact_phone1']()

    # FIXME: refactor all this, use sync/datamap at least for time
    st.fnClearTable()
    dict = global.dictValueCache
    progs = u.newModelDict "Program", true
    waz = u.newModelDict "Wazzup", true
    cities = u.newModelDict "City", true

    if phone
      $.getJSON( "/callsByPhone/#{phone}" )
      .done( (calls) ->
        return if _.isEmpty calls
        rows = for i of calls
          obj = calls[i]
          callDate = if obj.callDate
              new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm")
            else
              ''
          comment = []
          wazzup  = waz.getLab obj.wazzup
          comment.push("Что случилось: #{wazzup}") if wazzup

          callType = dict.CallTypes[obj.callType] || obj.callType || ''
          comment.push("Тип звонка: #{callType}") if callType

          comment.push("ФИО: #{obj.callerName_name}") if obj.callerName_name

          city = cities.getLab obj.city
          comment.push("Город: #{city}") if city

          program = progs.getLab obj.program
          comment.push("Программа: #{program}") if program
          callTaker = dict['users'][obj.callTaker] or ''
          comment.push("Сотрудник РАМК: #{callTaker}") if obj.callTaker
          row = [ callDate
                , callTaker
                , "звонок"
                , comment.join("<br/>")
                , ''
                ]
        st.fnAddData rows
      ).fail( (jqXHR, status, error) ->
        console.log "[#{status}] Can't load calls for '#{phone}' (#{error})"
      )

    $.getJSON( "/actionsFor/#{knockVM.id()}" )
    .done( (actions) ->
      return if _.isEmpty actions
      arDict = u.newModelDict "ActionResult", true
      atDict = u.newModelDict "ActionType", true
      rows = for r in actions
        result = arDict.getLab r.result
        name = atDict.getLab r.name
        aTo  = global.dictValueCache['users'][r.assignedTo] or
               r.assignedTo or ''
        time = if r.closeTime
               new Date(r.closeTime * 1000).toString("dd.MM.yyyy HH:mm")
        row = [ time or ''
              , aTo
              , name
              , r.comment or ''
              , result or '']
      st.fnAddData rows
    ).fail( (jqXHR, status, error) ->
      console.log "[#{status}] Can't load actions for '#{knockVM.id()}' (#{error})"
    )

    $.getJSON( "/cancelsFor/#{knockVM.id()}" )
    .done( (cancels) ->
      return if _.isEmpty cancels
      rows = for r in cancels
        ctime = new Date(r.ctime * 1000).toString("dd.MM.yyyy HH:mm")
        pname = r.partnerName
        reason = r.partnerCancelReason || ''
        owner  = dict['users'][r.owner] || r.owner
        comment = r.comment
        row = [ ctime
              , owner or ''
              , 'Отказ партнера'
              , pname + ': ' + comment
              , reason
              ]
      st.fnAddData rows
    ).fail( (jqXHR, status, error) ->
      console.log "[#{status}] Can't load cancels for '#{knockVM.id()}' (#{error})"
    )

    return if _.isEmpty knockVM['comments']()
    rows = for c in knockVM['comments']()
       [ c.date
       , c.user || ''
       , "Комментарий"
       , c.comment or ''
       , ""
       ]
    st.fnAddData rows


  descsKbHook: (model, knockVM) ->
    mkServicesDescs = (p, s) ->
      description: u.getServiceDesc(p, s.type())
      title:       s._meta.model.title
    knockVM['servicesDescs'] = ko.computed
      read: ->
        p = parseInt knockVM['program']?()
        s = knockVM['servicesReference']?()
        return [] unless p?
        _.chain(s).map((x) -> mkServicesDescs(p,x)).compact().value()
    knockVM['programDesc'] = ko.computed
      read: ->
        u.getProgramDesc (parseInt knockVM['program']()), (parseInt knockVM['subprogram']?())

  eventsHistoryKbHook: (model, knockVM) ->
    # History rendering is called from renderActions on caseScreen
    knockVM['fillEventHistory'] = fillEventsHistory(knockVM)
    knockVM['contact_phone1']?.subscribe fillEventsHistory(knockVM)
    knockVM['comments']?.subscribe fillEventsHistory(knockVM)

  # Display daily service stats in central pane when `city` field of
  # case is changed.
  cityStatsHook: (model, knockVM) ->
    cityField = "city"
    u.hideComplex
    knockVM[cityField]?.subscribe (new_city) ->
      $.getJSON "/stats/towAvgTime/" + new_city,
        (r) -> $("#city-towage-average-time").text(u.formatSecToMin(r[0]))

  regionHook: (model, knockVM) ->
    knockVM['region'] = ko.computed
      read: ->
        res = ''
        city = knockVM.city?()
        if city
          $.bgetJSON "/regionByCity/#{city}",
            (r) -> res = r.join ','
        res

  vwfakeHook: (model, knockVM) ->
    knockVM['callDateVisible'] = ko.computed ->
      not _.contains global.user.roles, global.idents("Role").vwfake

  carModelInfoHook: (model, knockVM) ->
    dict = new d.dicts.ModelDict
      dict: 'CarModel'
      meta:
        dictionaryKey: 'id'
        dictionaryLabel: 'info'
    knockVM['car_modelInfo'] = ko.computed ->
      dict.getLab knockVM['car_model']?()

  buttons: (model, kvm) ->
    return if /^search/.test(Finch.navigate())
    kvm.buttons = {}

    # Required fields for the needInfo button to be enabled
    niFlds = [ 'city'
             , 'contact_name'
             , 'contact_phone1'
             , 'customerComment'
             , 'program'
             ]

    kvm.buttons.needInfo = {}
    kvm.buttons.needInfo.tooltip = u.reqFieldsTooltip kvm, niFlds
    kvm.buttons.needInfo.text =
      u.newModelDict("CaseStatus").getLab(
              global.idents("CaseStatus").needInfo)
    kvm.buttons.needInfo.click = ->
      kvm['caseStatus'] global.idents("CaseStatus").needInfo
    kvm.buttons.needInfo.disabled = ko.computed ->
      vals = _.map niFlds, (n) -> kvm[n]?()
      empties = _.map vals, (e) -> e == "" || _.isNull e
      _.some empties

  hasFiles: (model, knockVM) ->
    knockVM['hasFiles'] = ko.computed ->
      knockVM['filesReference']?().length ||
      _.any(_.map(knockVM['servicesReference']?(),
        (srv) -> (srv['filesReference']?().length > 0)))
