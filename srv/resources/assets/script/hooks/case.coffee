define ["utils", "dictionaries"], (u, d) ->
  fillEventsHistory = (knockVM) -> ->

    # FIXME: hack to disable hook on newCase screen #1985
    return if /^newCase/.test(Finch.navigate())

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

          city = dict['DealerCities'][obj.city]
          comment.push("Город: #{city}") if city

          program = progs.getLab obj.program
          comment.push("Программа: #{program}") if program

          comment.push("Сотрудник РАМК: #{obj.callTaker}") if obj.callTaker
          row = [ callDate
                , obj.callTaker || ''
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
      rows = for r in actions
        result = dict.ActionResults[r.result] or ''
        name = dict.ActionNames[r.name] or ''
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
       , global.dictValueCache['users'][c.user] || ''
       , "Комментарий"
       , c.comment or ''
       , ""
       ]
    st.fnAddData rows


  descsKbHook: (model, knockVM) ->
    srvDict = new d.dicts.ModelDict
      dict: 'ServiceNames'
      meta:
        dictionaryLabel: 'value'
    mkServicesDescs = (p, s) ->
      description: u.getServiceDesc(p , srvDict.getVal s._meta.model.name)
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
    fillEventsHistory(knockVM)()
    knockVM['fillEventHistory'] = fillEventsHistory(knockVM)
    knockVM['contact_phone1']?.subscribe fillEventsHistory(knockVM)
    knockVM['actions']?.subscribe fillEventsHistory(knockVM)
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
