define ["utils", "text!tpl/screens/contract.html"], (utils, tpl) ->
  setupContracts = (viewName, args) ->
    $.getJSON "/all/contract", (contracts) ->
      for r in contracts
        r.name = '' unless r.name?
        r.templates = '' unless r.templates?
        r.id = (r.id.split ':')[1]
      global.contracts = contracts
      ko.applyBindings(global.contracts, el "layout" )
      utils.mkDataTable $('#contracts-table'), { sScrollY: '400px' }

      d1 = new Date
      d1.setDate 1
      d2 = new Date

      $('#date-from').val (d1.toString 'dd.MM.yyyy')
      $('#date-to').val (d2.toString 'dd.MM.yyyy')

  deleteReport = (e) ->
    return unless confirm "Вы уверены, что хотите удалить отчет?"
    objId = $(e).parents('tr').attr('id')
    $.ajax
      'type'     : 'DELETE'
      'url'      : "/_/contract/#{objId}"
      'success'  : -> forgetScreen(); renderScreen("contracts")
      'error'    : (xhr) -> console.log xhr; alert 'error'

  checkReportUniq = (ev) ->
    ev.preventDefault()
    name = $('#add-report input[name=name]').val()
    tpl  = $('#add-report input[name=templates]').val()
    if _.find(global.contracts, (e) -> e.name == name)
      alert "Отчет с таким именем уже существует."
    else if not name
      alert "Необходимо ввести название отчета!"
    else if not tpl
      alert "Необходимо добавить шаблон!"
    else
      $('#add-report').submit()

  { constructor: setupContracts
  , template   : tpl
  }