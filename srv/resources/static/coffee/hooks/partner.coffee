define ["utils"], (u) ->
  setServiceRepeat = ->
    kvm = global.viewsWare['partner-view'].knockVM
    return unless kvm
    refs = _.filter kvm.servicesReference(), (r) -> r.serviceName()
    groups = _.groupBy refs, (r) -> r.serviceName()
    r = (v[0].serviceNameLocal() for k, v of groups when v.length > 1)
    kvm.serviceRepeat(r)

  bindTitleServiceName: (model, kvm) ->
    kvm['modelTitle'] = kvm['serviceNameLocal']

  bindRemoveService: (model, kvm) ->
    kvm['services'].subscribe -> u.bindRemove kvm, 'services'

  serviceRepeat: (model, kvm) ->
    kvm['serviceRepeat'] = ko.observableArray([])
    kvm['services'].subscribe -> setServiceRepeat()

  partnerServiceRepeat: (model, kvm) ->
    kvm['serviceName'].subscribe -> setServiceRepeat()

  # Provides r/w access to first "fact" of partner's addresses
  factAddr: (model, kvm) ->
    addrs_field = 'addrs'
    fact_key = 'fact'
    kvm['factAddr'] =
      ko.computed
        read: ->
          u.getKeyedJsonValue kvm[addrs_field](), fact_key
        write: (val) ->
          json = kvm[addrs_field]()
          kvm[addrs_field] (u.setKeyedJsonValue json, fact_key, val)