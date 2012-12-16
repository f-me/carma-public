this.setupRKCScreen = (viewName, args) ->
  setTimeout ->
    caset = $("#rkc-services-table")
    actionst = $("#rkc-actions-table")

    return if caset.hasClass("dataTable")
    return if actionst.hasClass("dataTable")

    ct = mkDataTable caset, { bFilter: false, bInfo: false }
    bt = mkDataTable actionst, { bFilter: false, bInfo: false }

    totalServices = $('#total-services')
    averageStart = $('#average-towage-tech-start')
    calculated = $('#calculated-cost')
    mechanic = $('#mechanic')
    averageEnd = $('#average-towage-tech-end')
    limited = $('#limited-cost')

    satisfied = $('#satisfied-percentage')

    totalActions = $('#total-actions')
    totalIncompleteActions = $('#total-incomplete-actions')

    $('#reload').click -> update()

    dict = global.dictValueCache

    programs = for v in global.dictionaries.Programs.entries
      p =
        id: v.value
        name: v.label

    programs.unshift { id: "", name: "Все" }

    ko.applyBindings(programs, el("program-select"))

    cities = for v in global.dictionaries.DealerCities.entries
      c =
        id: v.value
        name: v.label

    cities.unshift { id: "", name: "Все" }

    ko.applyBindings(cities, el("city-select"))

    ps = $('#program-select')
    ps.change -> update()

    cs = $('#city-select')
    cs.change -> update()

    update = () ->
      prog = ps.val()
      city = cs.val()

      args = "?" + ["program=" + prog, "city=" + city].filter((x) -> x).join("&")

      $.getJSON("/rkc" + args, (result) ->
        dict = global.dictValueCache
        ct.fnClearTable()
        bt.fnClearTable()

        totalServices.val(result.case.summary.total)
        averageStart.val(Math.round(result.case.summary.delay / 60) + "m")
        calculated.val(result.case.summary.calculated)
        mechanic.val(result.case.summary.mech)
        averageEnd.val(Math.round(result.case.summary.duration / 60) + "m")
        limited.val(result.case.summary.limited)

        satisfied.val(result.case.summary.satisfied)

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

        brows = for binfo in result.back.actions
          brow = [
            dict.ActionNames[binfo.name] || binfo.name,
            binfo.total,
            binfo.undone,
            Math.floor(binfo.average / 60) + ":" + (binfo.average % 60)]

        bt.fnAddData(brows))

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
