this.setupRKCFrontScreen = (viewName, args) ->
  setTimeout ->

    callstable = $('#rkc-front-calls-table')
    return if callstable.hasClass("dataTable")
    callst = mkDataTable callstable, { bFilter: false, bInfo: false }

    getArgs = () -> this.filterRKCArgs()

    update = () ->
      args = getArgs()

      $.getJSON("/rkc/front" + args, (result) ->
        dict = global.dictValueCache
        callst.fnClearTable()
        summary = 0
        callst.fnAddData(
            for c in result.calls
                summary = summary + c.callcount
                callrow = [
                    dict.CallerTypes[c.callertype] || c.callertype || "Не указан",
                    dict.CallTypes[c.calltype] || c.calltype || "Не указан",
                    c.callcount])
        callst.fnAddData([[" Итого ", " Итого ", summary]]))

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
