define [], ->
  setServiceRepeat = ->
    kvm = global.viewsWare['partner-view'].knockVM
    return unless kvm
    refs = _.filter kvm.servicesReference(), (r) -> r.serviceName()
    groups = _.groupBy refs, (r) -> r.serviceName()
    r = (v[0].serviceNameLocal() for k, v of groups when v.length > 1)
    kvm.serviceRepeat(r)

  bindTitleServiceName: (instance, kvm) ->
    kvm['modelTitle'] = kvm['serviceNameLocal']

  bindRemoveService: (instance, kvm) ->
    kvm['services'].subscribe -> bindRemove kvm, 'services'

  serviceRepeat: (instance, kvm) ->
    kvm['serviceRepeat'] = ko.observableArray([])
    kvm['services'].subscribe -> setServiceRepeat()

  partnerServiceRepeat: (instance, kvm) ->
    kvm['serviceName'].subscribe -> setServiceRepeat()
