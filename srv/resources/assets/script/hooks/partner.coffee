define ["utils", "dictionaries/model-dict"], (u, ModelDict) ->
  bindTitleServiceName: (model, kvm) ->
    kvm['modelTitle'] = kvm['serviceNameLocal']

  tarifOptions: (model, kvm) ->
    if not /^partner/.test(Finch.navigate())
      return

    maybeNum = (str) -> if str then parseFloat str else null
    checkNum = (str) -> !isNaN(parseFloat str) && isFinite str
    # This converts svc["servicesModels"] (KVMs) to a JSON to be
    # persisted in services field of "partner" model
    syncJSON = ->
      # FIXME: if no errors
      kvm.services(
        kvm._serviceModels().map (s) ->
          type: s.type
          subtypes: s.subtypes?().map (st) ->
            subtype: st.subtype
            priority1: maybeNum st.priority1()
            priority2: maybeNum st.priority2()
            priority3: maybeNum st.priority3()
          priority1: maybeNum s.priority1()
          priority2: maybeNum s.priority2()
          priority3: maybeNum s.priority3()
          fine: maybeNum s.fine()
          options: s.options().map (o) ->
            name:   o.name()
            price1: maybeNum o.price1()
            price2: maybeNum o.price2()
      )

    subscribe = (k, callback) ->
      for f of k
        if ko.isObservable k[f]
          k[f].extend(rateLimit: 350)
          k[f].subscribe callback

    # Convert JSON into a KVM for a partner's service type
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
        optKVM._error = ko.computed ->
          [ !!optKVM.name()?.trim()   || 'name',
            checkNum(optKVM.price1()) || 'price1',
            checkNum(optKVM.price2()) || 'price2']
        optKVM.noError = ko.computed -> optKVM._error().every((e) -> e == true)
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
      validPriorities = [null,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

      # If the service type supports subtypes, add another level of
      # nesting: an observable array of objects with service subtypes,
      # each with its own priorities (priority1,2,3)
      sTypes = window.global.idents("ServiceType")
      if _.contains [sTypes.bikeTowage, sTypes.tech, sTypes.towage], svc.type
        svcKVM['subtypes'] = ko.observableArray []

        mkSubtypeDict = (name, meta) ->
          new ModelDict.dict({dict: name, meta: meta})

        # Pick a dictionary of service subtypes
        svcKVM['_subtypesDict'] =
          switch svc.type
            when sTypes.bikeTowage
              mkSubtypeDict("BikeTowType")
            when sTypes.tech
              mkSubtypeDict('TechType', {filterBy: 'isActive'})
            when sTypes.towage
              mkSubtypeDict('TowSort')
        # Do not display subtypes already added to the service
        svcKVM['_subtypesDict'].filteredItems = ko.computed ->
          svcKVM._subtypesDict.items.filter(
            (st) -> !svcKVM.subtypes().some((s) -> s.subtype == st.id)
          )

        # Convert JSON into a KVM for a partner's service subtype
        mkSubtypeSvc = (subtype, subtypeIx) ->
          subtypeKVM =
            index: subtypeIx
            subtype: subtype.subtype
            name: svcKVM['_subtypesDict'].getLab subtype.subtype
            priority1: ko.observable subtype.priority1
            priority2: ko.observable subtype.priority2
            priority3: ko.observable subtype.priority3
          subtypeKVM._error = ko.computed ->
            [ validPriorities.indexOf(maybeNum subtypeKVM.priority1()) >= 0 || 'priority1',
              validPriorities.indexOf(maybeNum subtypeKVM.priority2()) >= 0 || 'priority2',
              validPriorities.indexOf(maybeNum subtypeKVM.priority3()) >= 0 || 'priority3'
            ]
          subtypeKVM._delSubtype = () ->
            svcKVM['subtypes'].remove((s) -> s.index == subtypeIx)
            return false
          subscribe subtypeKVM, syncJSON
          return subtypeKVM

        # Create KVMs for existing subtypes
        svc.subtypes?.forEach((s, i) ->
          svcKVM['subtypes'].push(mkSubtypeSvc(s, i)))

        svcKVM['_addSubtype'] = (e) ->
          svcSubtype = e.id
          svcKVM['subtypes'].push(
            mkSubtypeSvc({subtype: svcSubtype}, svcKVM['subtypes']().length)
          )
          return false
      # Non-subtyped service
      else
        svcKVM['subtypes'] = null
        svcKVM['_subtypesDict'] = {}
        svcKVM['_addSubtype'] = () -> return false

      svcKVM._error = ko.computed ->
        [ validPriorities.indexOf(maybeNum svcKVM.priority1()) >= 0 || 'priority1',
          validPriorities.indexOf(maybeNum svcKVM.priority2()) >= 0 || 'priority2',
          validPriorities.indexOf(maybeNum svcKVM.priority3()) >= 0 || 'priority3',
          checkNum(svcKVM.fine())                                   || 'fine',
          svcKVM.options().every((o) -> o.noError())                || 'options',
          !kvm._serviceModels().some(
            (s) -> s.type == svc.type && s.index != svcIx)          || 'duplicate']
      svcKVM.noError = ko.computed -> svcKVM._error().every((e) -> e == true)
      subscribe svcKVM, syncJSON
      return svcKVM


    kvm['_serviceTypes'] = new ModelDict.dict(dict: 'ServiceType')

    kvm['_serviceModels'] = ko.observableArray []
    kvm.services()?.forEach((s,i) -> kvm._serviceModels.push(mkSvc(s, i)))
    kvm['_serviceTypes'].filteredItems = ko.computed ->
      kvm._serviceTypes.items.filter((t) -> !kvm._serviceModels().some((s) -> s.type == t.id))

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
