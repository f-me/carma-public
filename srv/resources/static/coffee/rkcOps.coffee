this.setupRKCOpsScreen = (viewName, args) ->
  setTimeout ->

    d1 = new Date
    d2 = new Date
    d2.setDate (d1.getDate() + 1)

    dateFrom = $('#rkc-date-from')
    dateTo = $('#rkc-date-to')

    dateFrom.val (d1.toString 'dd.MM.yyyy')
    dateTo.val (d2.toString 'dd.MM.yyyy')

    eachao = $('#rkc-ops-back-operators-table')

    return if eachao.hasClass("dataTable")

    actstbl = {}
    actstbl.cols = for v in global.dictionaries.ActionNames.entries
        c =
            name: v.label

    actstbl.cols.unshift { name: "Среднее время обработки действия" }
    actstbl.cols.unshift { name: "Оператор" }

    ko.applyBindings(actstbl, el("rkc-ops-back-operators-table"))

    eat = mkDataTable eachao, { bFilter: false, bInfo: false }

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

    dateFrom.change -> update()
    dateTo.change -> update()

    fmttime = (tm) ->
        fmt = (x) -> if x < 10 then "0" + x else "" + x
        Math.floor(tm / 60) + ":" + fmt(tm % 60)

    fmtavg = (val) ->
        fmttime(val[0]) + "/" + val[1]

    update = () ->
      prog = ps.val()
      city = cs.val()

      from = dateFrom.val()
      to = dateTo.val()

      args = "?" + ["program=" + prog, "city=" + city, "from=" + from, "to=" + to].filter((x) -> x).join("&")

      $.getJSON("/rkc" + args, (result) ->
        dict = global.dictValueCache
        eat.fnClearTable()

        eavision = []
        eavision.length = actstbl.cols.length
        eavision[0] = true
        eavision[1] = true

        earows = for eainfo in result.eachopactions
            r = for val, i in eainfo.avgs
                if val
                    eavision[i + 2] = true
                if val then fmtavg(val) else "-"
            r.unshift fmtavg(eainfo.avg)
            r.unshift eainfo.name
            earow = r

        for c, i in eavision
            eat.fnSetColumnVis(i, if c then true else false)

        eat.fnAddData(earows))

    global.rkcOpsData = {}

    global.rkcOpsData.updateHandler = setInterval(update, 30000)

    update()

this.removeRKCOpsScreen = ->
    t = global.rkcOpsData.updateHandler
    clearInterval t if t?
