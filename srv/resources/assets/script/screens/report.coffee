define [ "utils"
       , "render/screen"
       , "text!tpl/screens/reports.html"
       ],
  (utils, render, tpl) ->
    setupReports = (viewName, args) ->
      $.getJSON "/all/report", (reports) ->
        for r in reports
          r.name = '' unless r.name?
          r.templates = '' unless r.templates?
          r.id = (r.id.split ':')[1]
        global.reports = reports
        ko.applyBindings(global.reports, el "layout" )
        utils.mkDataTable $('#reports-table'), { sScrollY: '400px' }

        d1 = new Date
        d1.setDate 1
        d2 = new Date

        $('#date-from').val (d1.toString 'dd.MM.yyyy')
        $('#date-to').val (d2.toString 'dd.MM.yyyy')

    utils.build_global_fn 'deleteReport', ['screens/report']

    deleteReport = (e) ->
      return unless confirm "Вы уверены, что хотите удалить отчет?"
      objId = $(e).parents('tr').attr('id')
      $.ajax
        'type'     : 'DELETE'
        'url'      : "/_/report/#{objId}"
        'success'  : ->
          render.forgetScreen()
          render.renderScreen("reports", obj)
        'error'    : (xhr) -> console.log xhr; alert 'error'

    checkReportUniq = (ev) ->
      ev.preventDefault()
      name = $('#add-report input[name=name]').val()
      tpl  = $('#add-report input[name=templates]').val()
      if _.find(global.reports, (e) -> e.name == name)
        alert "Отчет с таким именем уже существует."
      else if not name
        alert "Необходимо ввести название отчета!"
      else if not tpl
        alert "Необходимо добавить шаблон!"
      else
        $('#add-report').submit()

    obj =
      constructor : setupReports
      template    : tpl
      deleteReport: deleteReport
    return obj
