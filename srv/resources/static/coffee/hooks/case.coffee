define ["utils"], (u) ->
  fillEventsHistory = (knockVM) -> ->
    t = $("#call-searchtable")
    st = t.dataTable()
    # return if table template is not yet rendered
    return unless $("#call-searchtable")[0]

    phone = knockVM['contact_phone1']()
    $.getJSON "/callsByPhone/#{phone}", (calls) ->
      $.getJSON "/actionsFor/#{knockVM.id()}", (actions) ->
        st.fnClearTable()
        dict = global.dictValueCache

        for i of calls
          obj = calls[i]
          callDate = if obj.callDate
              new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm")
            else
              ''
          comment = []
          wazzup  = dict.Wazzup[obj.wazzup] || obj.wazzup || ''
          comment.push("Что случилось: #{wazzup}") if wazzup
          callType = dict.CallerTypes[obj.callType] || obj.callType || ''
          comment.push("Тип звонка: #{callType}") if callType
          comment.push("ФИО: #{obj.callerName_name}") if obj.callerName_name
          city = dict['DealerCities'][obj.city]
          comment.push("Город: #{city}") if city
          program = global.dictionaries['Programs'][obj.program]
          comment.push("Программа: #{program}") if program
          make = dict['CarMakers'][obj.make]
          comment.push("Марка: #{make}") if make
          model = dict['CarModels'][obj.model]
          comment.push("Модель: #{model}") if model
          comment.push("Сотрудник РАМК: #{obj.callTaker}") if obj.callTaker
          row = [ callDate
                , obj.callTaker || ''
                , "звонок"
                , comment.join("<br/>")
                , ''
                ]

          st.fnAddData(row)

        for r in actions
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
                , result ]

          st.fnAddData(row)

        return if _.isEmpty knockVM['comments']()
        for c in knockVM['comments']()
          st.fnAddData [ c.date
                       , global.dictValueCache['users'][c.user] || ''
                       , "Комментарий"
                       , c.comment
                       , ""
                       ]


  descsKbHook: (instance, knockVM) ->
    mkServicesDescs = (p, s) ->
      description: u.getServiceDesc(p ,s.modelName())
      title:       s.modelTitle
    knockVM['servicesDescs'] = ko.computed
      read: ->
        p = knockVM['program']()
        s = knockVM['servicesReference']()
        return [] unless p?
        _.chain(s).map((x) -> mkServicesDescs(p,x)).compact().value()
    knockVM['programDesc'] = ko.computed
      read: ->
        global.dictionaries['ProgramInfo'][knockVM['program']()]

  eventsHistoryKbHook: (instance, knockVM) ->
    knockVM['contact_phone1'].subscribe fillEventsHistory(knockVM)
    knockVM['actions'].subscribe fillEventsHistory(knockVM)
    knockVM['comments'].subscribe fillEventsHistory(knockVM)

  # Display daily service stats in central pane when `city` field of
  # case is changed.
  cityStatsHook: (instance, knockVM) ->
    cityField = "city"
    u.hideComplex
    knockVM[cityField].subscribe (new_city) ->
      $.getJSON "/stats/towAvgTime/" + new_city,
        (r) -> $("#city-towage-average-time").text(u.formatSecToMin(r[0]))
