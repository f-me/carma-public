this.setupRKCScreen = (viewName, args) ->
  setTimeout ->
    caset = $("#rkc-services-table")
    frontt = $('#rkc-operators-table')
    backt = $("#rkc-back-office-table")
    eachao = $('#rkc-each-action-op-avg-table')

    return if caset.hasClass("dataTable")
    return if frontt.hasClass('dataTable')
    return if backt.hasClass("dataTable")
    return if eachao.hasClass("dataTable")

    actstbl = {}
    actstbl.cols = for v in global.dictionaries.ActionNames.entries
        c =
            name: v.label

    actstbl.cols.unshift { name: "Оператор" }

    ko.applyBindings(actstbl, el("rkc-each-action-op-avg-table"))

    ct = mkDataTable caset, { bFilter: false, bInfo: false }
    ft = mkDataTable frontt, { bFilter: false, bInfo: false }
    bt = mkDataTable backt, { bFilter: false, bInfo: false }
    eat = mkDataTable eachao, { bFilter: false, bInfo: false }

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
        ft.fnClearTable()
        bt.fnClearTable()
        eat.fnClearTable()

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

        frows = for finfo in result.front.operators
          frow = [
            finfo.name,
            finfo.roles,
            Math.floor(finfo.avg / 60) + ":" + (finfo.avg % 60)]

        ft.fnAddData(frows)

        totalActions.val(result.back.summary.total)
        totalIncompleteActions.val(result.back.summary.undone)

        brows = for binfo in result.back.actions
          brow = [
            dict.ActionNames[binfo.name] || binfo.name,
            binfo.total,
            binfo.undone,
            Math.floor(binfo.average / 60) + ":" + (binfo.average % 60)]

        bt.fnAddData(brows)

        eavision = []
        eavision.length = actstbl.cols.length
        eavision[0] = true

        earows = for eainfo in result.eachopactions
            r = for val, i in eainfo.avgs
                if val
                    eavision[i + 1] = true
                if val then Math.floor(val / 60) + ":" + (val % 60) else "-"
            r.unshift eainfo.name
            earow = r
            # eainfo.avgs.unshift eainfo.name
            # earow = eainfo.avgs

        for c, i in eavision
            eat.fnSetColumnVis(i, if c then true else false)

        eat.fnAddData(earows))

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
