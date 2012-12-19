this.setupRKCOpsScreen = (viewName, args) ->
  setTimeout ->
    frontt = $('#rkc-ops-front-operators-table')
    eachao = $('#rkc-ops-back-operators-table')

    return if frontt.hasClass('dataTable')
    return if eachao.hasClass("dataTable")

    actstbl = {}
    actstbl.cols = for v in global.dictionaries.ActionNames.entries
        c =
            name: v.label

    actstbl.cols.unshift { name: "Оператор" }

    ko.applyBindings(actstbl, el("rkc-ops-back-operators-table"))

    ft = mkDataTable frontt, { bFilter: false, bInfo: false }
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

    update = () ->
      prog = ps.val()
      city = cs.val()

      args = "?" + ["program=" + prog, "city=" + city].filter((x) -> x).join("&")

      $.getJSON("/rkc" + args, (result) ->
        dict = global.dictValueCache
        ft.fnClearTable()
        eat.fnClearTable()

        frows = for finfo in result.front.operators
          frow = [
            finfo.name,
            finfo.roles,
            Math.floor(finfo.avg / 60) + ":" + (finfo.avg % 60)]

        ft.fnAddData(frows)

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

    global.rkcOpsData = {}

    global.rkcOpsData.updateHandler = setInterval(update, 30000)

    update()

this.removeRKCOpsScreen = ->
    t = global.rkcOpsData.updateHandler
    clearInterval t if t?
