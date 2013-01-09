
initReducedModeBtn = ->
  currentState = false
  btn = $('#rkc-ReducedActionsMode')
  updState = (fs) ->
      currentState = _.contains fs, "ReducedActionsMode"
      btnName = if currentState then "Выключить" else "Включить"
      btn.text btnName

  $.getJSON '/runtimeFlags', updState

  btn.click ->
    $.ajax
      type: 'PUT'
      url: '/runtimeFlags'
      data: "{\"ReducedActionsMode\": #{not currentState}}"
      success: updState

this.rkcWeatherAddCity = (name) ->
  dict = global.dictValueCache
  $.getJSON('/rkc/weather/' + name, (result) ->
    $('#rkc-weather-table').dataTable().fnAddData([dict.DealerCities[name], result, name]))

this.updatePartners = (partners) ->
  setTimeout ->
    dateFrom = $('#rkc-date-from')
    dateTo = $('#rkc-date-to')

    from = dateFrom.val()
    to = dateTo.val()

    partnerArgs = "?" + ["from=" + from, "to=" + to].filter((x) -> x).join("&")

    $.getJSON("/rkc/partners" + partnerArgs, (result) ->
      partners.removeAll()
      partners.push({ id: "", name: "Все" })
      for r in result
        partners.push({ id: r, name: r }))

this.initRKCDate = (updater, partners) ->
  setTimeout ->
    d1 = new Date
    d2 = new Date
    d2.setDate (d1.getDate() + 1)

    dateFrom = $('#rkc-date-from')
    dateTo = $('#rkc-date-to')

    dateFrom.val (d1.toString 'dd.MM.yyyy')
    dateTo.val (d2.toString 'dd.MM.yyyy')

    updateps = () -> this.updatePartners(partners)

    dateFrom.change ->
      updateps()
      updater()
    dateTo.change ->
      updateps()
      updater()

this.fillRKCFilters = (updater, partners) ->
  setTimeout ->
    dict = global.dictValueCache

    # Fill programs
    programs = for v in global.dictionaries.Programs.entries
      p =
        id: v.value
        name: v.label

    programs.unshift { id: "", name: "Все" }

    ko.applyBindings(programs, el("program-select"))

    # Fill cities
    cities = for v in global.dictionaries.DealerCities.entries
      c =
        id: v.value
        name: v.label

    cities.unshift { id: "", name: "Все" }

    ko.applyBindings(cities, el("city-select"))

    # Fill partner
    partners.push({ id: "", name: "Все" })

    ko.applyBindings(partners, el("partner-select"))

    # Init weather table
    this.rkcWeatherAddCity('Moskva')
    this.rkcWeatherAddCity('Sankt-Peterburg')

    # Set on-change
    ps = $('#program-select')
    ps.change updater

    cs = $('#city-select')
    cs.change updater

    pps = $('#partner-select')
    pps.change updater

    $('#reload').click updater

this.filterRKCArgs = () ->
  prog = $('#program-select').val()
  city = $('#city-select').val()

  partner = $('#partner-select').val()

  from = $('#rkc-date-from').val()
  to = $('#rkc-date-to').val()

  args = "?" + ["program=" + prog, "city=" + city, "partner=" + partner, "from=" + from, "to=" + to].filter((x) -> x).join("&")

  return args

this.setupRKCScreen = (viewName, args) ->
  setTimeout ->
    initReducedModeBtn()

    caset = $("#rkc-services-table")
    actionst = $("#rkc-actions-table")
    weathert = $('#rkc-weather-table')

    return if caset.hasClass("dataTable")
    return if actionst.hasClass("dataTable")
    return if weathert.hasClass("dataTable")

    ct = mkDataTable caset, { bFilter: false, bInfo: false }
    bt = mkDataTable actionst, { bFilter: false, bInfo: false }
    wt = mkDataTable weathert, {
      bFilter: false,
      bInfo: false,
      fnRowCallback: (nRow, aData, iDisplayIndex, iDisplayIndexFull) ->
        delBtn = $('td:eq(2)', nRow)
        delBtn.html "<button class=\"btn\">Удалить</button>"
        delBtn.off('click')
        delBtn.on('click', -> wt.fnDeleteRow nRow) }

    # Fill general info
    totalServices = $('#total-services')
    averageStart = $('#average-towage-tech-start')
    calculated = $('#calculated-cost')
    mechanic = $('#mechanic')
    averageEnd = $('#average-towage-tech-end')
    limited = $('#limited-cost')

    satisfied = $('#satisfied-percentage')

    totalActions = $('#total-actions')
    totalIncompleteActions = $('#total-incomplete-actions')

    # Fill weather cities
    cities = for v in global.dictionaries.DealerCities.entries
      c =
        id: v.value
        name: v.label

    ko.applyBindings(cities, el "rkc-weather-city-select")

    fmttime = (tm) ->
        fmt = (x) -> if x < 10 then "0" + x else "" + x
        Math.floor(tm / 60) + ":" + fmt(tm % 60)

    getArgs = () -> this.filterRKCArgs()

    update = () ->
      args = getArgs()

      $.getJSON("/rkc" + args, (result) ->
        dict = global.dictValueCache
        ct.fnClearTable()
        bt.fnClearTable()

        # Update general info
        totalServices.val(result.case.summary.total)
        averageStart.val(Math.round(result.case.summary.delay / 60) + "m")
        calculated.val(result.case.summary.calculated)
        mechanic.val(result.case.summary.mech)
        averageEnd.val(Math.round(result.case.summary.duration / 60) + "m")
        limited.val(result.case.summary.limited)

        satisfied.val(result.case.summary.satisfied)

        # Update services table
        crows = for cinfo in result.case.services
          crow = [
            dict.Services[cinfo.name] || cinfo.name,
            cinfo.total,
            Math.round(cinfo.delay / 60) + "m",
            Math.round(cinfo.duration / 60) + "m",
            cinfo.calculated,
            cinfo.limited]

        ct.fnAddData(crows)

        totalActions.val(result.back.summary.total)
        totalIncompleteActions.val(result.back.summary.undone)

        # Update actions table
        brows = for binfo in result.back.actions
          brow = [
            dict.ActionNames[binfo.name] || binfo.name,
            binfo.total,
            binfo.undone,
            fmttime(binfo.average)]

        bt.fnAddData(brows))

    partners = ko.observableArray([])
    this.initRKCDate update, partners
    this.fillRKCFilters update, partners

    global.rkcData = {}

    # Get SMS
    sms = $('#sms-processing')

    updateSMS = () ->
        $.getJSON("/sms/processing", (result) ->
            sms.val(result.processing))

    global.rkcData.smsHandler = setInterval(updateSMS, 5000)
    global.rkcData.updateHandler = setInterval(update, 30000)

    updateSMS()
    update()

this.removeRKCScreen = ->
    h = global.rkcData.smsHandler
    clearInterval h if h?
    t = global.rkcData.updateHandler
    clearInterval t if t?

this.rkcWeatherAddSelectedCity = ->
  this.rkcWeatherAddCity $('#rkc-weather-city-select').val()
