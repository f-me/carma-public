define ["dictionaries", "text!tpl/screens/printSrv.html"], (D, tpl) ->

  # FIXME: rewrite this screen with models and all dat cool stuff

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
          Program  : 'program'
          Wazzup   : 'comment'
          CarMake  : 'car_make'
          CarModel : 'car_model'
          Usermeta : 'callTaker'
          ContractCheckStatus : 'vinChecked'
      postProc arg.service,
        time: ['createTime', 'times_factServiceStart', 'times_factServiceEnd']
        lookup:
          Services        : 'type'
          ServiceStatus   : 'status'
          FalseStatuses   : 'falseCall'

      for s in (arg.cancels || [])
        s.service = s.serviceid
        postProc s,
          lookup:
            users                : 'owner'
            PartnerRefusalReason : 'partnerCancelReason'
            Services             : 'service'
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
    # little hack to make it work
    newDicts = [ "Program"
               , "Wazzup"
               , "PartnerRefusalReason"
               , "ContractCheckStatus"
               , "ServiceStatus"
               , "CarMake"
               , "CarModel"
               , "Usermeta"
               ]
    if _.contains newDicts, dict
      label = if dict == 'Usermeta' then 'realName' else 'label'
      opts  = dict: dict, meta: {dictionaryLabel: label}
      (new D.dicts.ModelDict(opts)).getLab val
    else
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
