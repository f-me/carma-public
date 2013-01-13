this.setupRKCFrontScreen = (viewName, args) ->
  setTimeout ->

    callstable = $('#rkc-front-calls-table')
    opstable = $('#rkc-front-ops-table')

    return if callstable.hasClass("dataTable")
    return if opstable.hasClass("dataTable")

    callst = mkDataTable callstable, { bFilter: false, bInfo: false }
    opst = mkDataTable opstable, { bFilter: false, bInfo: false }

    getArgs = () -> this.filterRKCArgs()

    update = () ->
      args = getArgs()

      $.getJSON("/rkc/front" + args, (result) ->
        dict = global.dictValueCache

        callst.fnClearTable()
        opst.fnClearTable()

        summary = 0
        callst.fnAddData(
            for c in result.calls
                summary = summary + c.callcount
                callrow = [
                    dict.CallerTypes[c.callertype] || c.callertype || "Не указан",
                    dict.CallTypes[c.calltype] || c.calltype || "Не указан",
                    c.callcount])
        callst.fnAddData([[" Итого ", " Итого ", summary]])

        opst.fnAddData(
            for o in result.ops
                opsrow = [
                    o.name || "Не указано",
                    o.calls,
                    o.cases]))

    partners = ko.observableArray([])
    this.initRKCDate update, partners
    this.fillRKCFilters update, partners

    global.rkcFrontData = {}

    global.rkcFrontData.updateHandler = setInterval(update, 30000)

    update()
    this.updatePartners(partners)

this.removeRKCFrontScreen = ->
    t = global.rkcFrontData.updateHandler
    clearInterval t if t?
