this.setupRKCScreen = (viewName, args) ->
  setTimeout ->
    t = $("#rkc-services-table")
    return if t.hasClass("dataTable")
    dt = mkDataTable t, { bFilter : false, bInfo : false }

    totalServices = $('#total-services')
    averageStart = $('#average-towage-tech-start')
    calculated = $('#calculated-cost')
    mechanic = $('#mechanic')
    averageEnd = $('#average-towage-tech-end')
    limited = $('#limited-cost')

    satisfied = $('#satisfied-percentage')

    $('#reload').click -> update()

    dict = global.dictValueCache

    programs = for v in global.dictionaries.Programs.entries
        p =
            id: v.value
            name: v.label

    programs.unshift { id: "", name: "Все" }

    ko.applyBindings(programs, el("program-select"))

    ps = $('#program-select')

    ps.change -> update()

    update = () ->
      prog = ps.val()
      $.getJSON("/rkc/" + prog, (result) ->
        dict = global.dictValueCache
        dt.fnClearTable()

        totalServices.val(result.summary.total)
        averageStart.val(Math.round(result.summary.delay / 60) + "m")
        calculated.val(result.summary.calculated)
        mechanic.val(result.summary.mech)
        averageEnd.val(Math.round(result.summary.duration / 60) + "m")
        limited.val(result.summary.limited)

        satisfied.val(result.summary.satisfied)

        rows = for info in result.services
          row = [
            dict.Services[info.name] || info.name,
            info.total,
            Math.round(info.delay / 60) + "m",
            Math.round(info.duration / 60) + "m",
            info.calculated,
            info.limited]

        dt.fnAddData(rows))

    setTimeout update, 30000

    update()
