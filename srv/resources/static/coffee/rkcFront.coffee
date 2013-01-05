this.setupRKCFrontScreen = (viewName, args) ->
  setTimeout ->

    d1 = new Date
    d2 = new Date
    d2.setDate (d1.getDate() + 1)

    dateFrom = $('#rkc-date-from')
    dateTo = $('#rkc-date-to')

    dateFrom.val (d1.toString 'dd.MM.yyyy')
    dateTo.val (d2.toString 'dd.MM.yyyy')

    callstable = $('#rkc-front-calls-table')

    return if callstable.hasClass("dataTable")

    callst = mkDataTable callstable, { bFilter: false, bInfo: false }

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

    update = () ->
      prog = ps.val()
      city = cs.val()

      from = dateFrom.val()
      to = dateTo.val()

      args = "?" + ["program=" + prog, "city=" + city, "from=" + from, "to=" + to].filter((x) -> x).join("&")

      $.getJSON("/rkc/front" + args, (result) ->
        dict = global.dictValueCache
        callst.fnClearTable()
        callst.fnAddData(
            for c in result.calls
                callrow = [
                    dict.CallerTypes[c.callertype] || c.callertype,
                    dict.CallTypes[c.calltype] || c.calltype,
                    c.callcount]))

    global.rkcFrontData = {}

    global.rkcFrontData.updateHandler = setInterval(update, 30000)

    update()

this.removeRKCFrontScreen = ->
    t = global.rkcFrontData.updateHandler
    clearInterval t if t?
