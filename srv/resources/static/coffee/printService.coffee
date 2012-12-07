this.setupPrintSrv = (viewName, {model: model, id: id}) ->
  $(".navbar").hide()
  $.getJSON "/printSrv/#{model}/#{id}", (arg) ->
    arg.action.assignedTo = lookup('users', arg.action.assignedTo)
    arg.service.type = model
    postProc arg.kase,
      time: ['callDate']
      lookup:
        Programs   : 'program'
        Wazzup     : 'comment'
        CarMakers  : 'car_make'
        CarModels  : 'car_model'
        VINChecked : 'vinChecked'
    postProc arg.service,
      time: ['createTime', 'times_factServiceStart', 'times_factServiceEnd']
      lookup:
        Services        : 'type'
        ServiceStatuses : 'status'

    ko.applyBindings arg, el("print-table")

this.destroyPrintSrv = () ->
  $(".navbar").show()

this.openPrintAction = (kvm) ->
  return unless kvm.id()
  window.location.hash = "printAction/#{kvm.id()}"

time = (time) ->
  return "" if _.isEmpty(time)
  new Date(time * 1000).toString("dd.MM.yyyy HH:mm")

lookup = (dict, val) ->
  global.dictValueCache[dict][val] || ''

postProc = (obj, procs) ->
  for t in procs.time
    obj[t] = time(obj[t])
  for d, f of procs.lookup
    obj[f] = lookup(d, obj[f]) || ''
