this.setupRKCScreen = (viewName, args) ->
  setTimeout ->
    t = $("#rkc-services-table")
    return if t.hasClass("dataTable")
    dt = mkDataTable t

    totalServices = $('#total-services')
    averageStart = $('#average-towage-tech-start')
    calculated = $('#calculated-cost')
    mechanic = $('#mechanic')
    averageEnd = $('#average-towage-tech-end')
    limited = $('#limited-cost')

    satisfied = $('#satisfied-percentage')

    $('#reload').click -> update()

    update = () ->
      $.getJSON("/rkc", (result) ->
        dict = global.dictValueCache
        dt.fnClearTable()

        totalServices.val(result.summary.total)
        averageStart.val((result.summary.delay / 60) + "m")
        calculated.val(result.summary.calculated)
        mechanic.val(result.summary.mech)
        averageEnd.val((result.summary.duration / 60) + "m")
        limited.val(result.summary.limited)

        satisfied.val(result.summary.satisfied)

        rows = for info in result.services
          row = [
            dict.Services[info.name] || info.name,
            info.total,
            (info.delay / 60) + "m",
            (info.duration / 60) + "m",
            info.calculated,
            info.limited]

        dt.fnAddData(rows))

    update()
