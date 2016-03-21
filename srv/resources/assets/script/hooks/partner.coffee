define ["utils", "dictionaries/model-dict"], (u, ModelDict) ->
  bindTitleServiceName: (model, kvm) ->
    kvm['modelTitle'] = kvm['serviceNameLocal']

  tarifOptions: (model, kvm) ->
    if not /^partner/.test(Finch.navigate())
      return

    maybeInt = (str) -> if str then parseInt str else null
    syncJSON = ->
      # FIXME: if no errors
      kvm.services(
        kvm._serviceModels().map (s) ->
          type: s.type
          priority1: maybeInt s.priority1()
          priority2: maybeInt s.priority2()
          priority3: maybeInt s.priority3()
          fine: maybeInt s.fine()
          options: s.options().map (o) ->
            name:   o.name()
            price1: maybeInt o.price1()
            price2: maybeInt o.price2()
      )

    subscribe = (k, callback) ->
      for f of k
        if ko.isObservable k[f]
          k[f].extend(rateLimit: 350)
          k[f].subscribe callback

    mkSvc = (svc, svcIx) ->
      options = ko.observableArray []

      mkOpt = (opt, optIx) ->
        optKVM =
          index:  optIx
          name:   ko.observable opt.name
          price1: ko.observable opt.price1
          price2: ko.observable opt.price2
          delOption: ->
            options.remove((o) -> o.index == optIx)
            return false
        subscribe optKVM, syncJSON
        return optKVM

      svc.options?.forEach((o,i) -> options.push(mkOpt(o, i)))

      svcKVM =
        index: svcIx
        delService: ->
          kvm._serviceModels.remove((s) -> s.index == svcIx)
          return false
        addOption: ->
          options.push(mkOpt({}, options().length))
          return false
        type: svc.type
        name: kvm._serviceTypes.getLab svc.type
        priority1: ko.observable svc.priority1
        priority2: ko.observable svc.priority2
        priority3: ko.observable svc.priority3
        fine: ko.observable svc.fine
        options: options
      subscribe svcKVM, syncJSON
      return svcKVM


    kvm['_serviceTypes'] = new ModelDict.dict(dict: 'ServiceType')

    kvm['_serviceModels'] = ko.observableArray []
    kvm.services()?.forEach((s,i) -> kvm._serviceModels.push(mkSvc(s, i)))

    kvm['_addService'] = (svcType) -> ->
      kvm._serviceModels.push mkSvc({type: svcType}, kvm._serviceModels().length)
      return false

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
