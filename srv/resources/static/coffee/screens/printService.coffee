define ["text!tpl/screens/printSrv.html"], (tpl) ->
  setupPrintSrv = (viewName, {model: model, id: id}) ->
    $(".navbar").hide()

    $.getJSON "/printSrv/#{model}/#{id}", (arg) ->

      arg.service = arg.service[0]
      arg.kase    = arg.kase[0]

      arg.service.assignedTo = lookup('users', arg.service.assignedTo)
      arg.service.type = model
      postProc arg.kase,
        time: ['callDate']
        lookup:
          Program    : 'program'
          Wazzup     : 'comment'
          CarMakers  : 'car_make'
          CarModels  : 'car_model'
          VINChecked : 'vinChecked'
      postProc arg.service,
        time: ['createTime', 'times_factServiceStart', 'times_factServiceEnd']
        lookup:
          Services        : 'type'
          ServiceStatuses : 'status'
          FalseStatuses   : 'falseCall'

      for s in (arg.cancels || [])
        s.service = s.serviceid.split(':')[0]
        postProc s,
          lookup:
            users               : 'owner'
            PartnerCancelReason : 'partnerCancelReason'
            Services            : 'service'
          time: ['ctime']

      arg.kase.comments = $.parseJSON arg.kase.comments
      for s in (arg.kase.comments || [])
        postProc s,
          lookup:
            users: 'user'

      ko.applyBindings arg, el("print-table")

  destroyPrintSrv = () ->
    $(".navbar").show()

  openPrintAction = (kvm) ->
    return unless kvm.id()
    window.location.hash = "printAction/#{kvm.id()}"

  time = (time) ->
    return "" if _.isEmpty(time)
    new Date(time * 1000).toString("dd.MM.yyyy HH:mm")

  lookup = (dict, val) ->
    global.dictValueCache[dict][val] || ''

  postProc = (obj, procs) ->
    if procs.time
      for t in procs.time
        obj[t] = time(obj[t])
    if procs.lookup
      for d, f of procs.lookup
        obj[f] = lookup(d, (obj[f] || '')) || obj[f] || ''

  { constructor: setupPrintSrv
  , destructor: destroyPrintSrv
  , template: tpl
  }
