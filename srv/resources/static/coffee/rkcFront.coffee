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
    this.updatePartners(partners)

this.removeRKCFrontScreen = ->
    t = global.rkcFrontData.updateHandler
    clearInterval t if t?
