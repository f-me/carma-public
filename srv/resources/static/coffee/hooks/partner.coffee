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
