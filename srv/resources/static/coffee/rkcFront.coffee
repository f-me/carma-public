this.setupRKCFrontScreen = (viewName, args) ->
  setTimeout ->

    dateFrom = $('#rkc-date-from')
    dateTo = $('#rkc-date-to')

    callstable = $('#rkc-front-calls-table')

    return if callstable.hasClass("dataTable")

    callst = mkDataTable callstable, { bFilter: false, bInfo: false }

    dict = global.dictValueCache

    ps = $('#program-select')
    cs = $('#city-select')

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

    partners = ko.observableArray([])
    this.initRKCDate update, partners
    this.fillRKCFilters update, partners

    global.rkcFrontData = {}

    global.rkcFrontData.updateHandler = setInterval(update, 30000)

    update()

this.removeRKCFrontScreen = ->
    t = global.rkcFrontData.updateHandler
    clearInterval t if t?
