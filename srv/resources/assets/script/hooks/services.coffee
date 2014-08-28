define [ "utils"
       , "model/utils"
       , "screens/partnersSearch"
       ], (u, mu, pSearch) ->
  # sync with partner search screen
  openPartnerSearch: (model, kvm) ->
    # do not run this hook on search screen
    return if /^search/.test(Finch.navigate())

    # subscibe partner fields to partnersSearch screen events
    for f in model.fields when f.meta?.widget == "partner"
      do (f) ->
        n = pSearch.subName f.name, model.name, kvm.id()
        global.pubSub.sub n, (val) ->
          kvm[f.name](val.name)
          kvm["#{f.name}Id"]?("partner:#{val.id}")
          addr = val.addrDeFacto
          field_basename = f.name.split('_')[0]
          # Replace group names:
          #
          # contractor -> towerAddress, towDealer -> towAddress
          #
          # for subfields _coords and _address. If such fields exist,
          # copy data to them too.
          field_subname =
            switch field_basename
              when "contractor" then "towerAddress"
              when "towDealer"  then "towAddress"
              else field_basename
          kvm["#{field_subname}_address"]?(addr || "")
          kvm["#{field_subname}_coords"]? val.coords
          kvm["#{field_basename}_address"]?(addr || "")
          kvm["#{field_basename}_coords"]? val.coords
          if (field_basename == "towDealer") && val.distanceFormatted?
            kvm["dealerDistance"](val.distanceFormatted)
          kvm['_parent']['fillEventHistory']()

    # this fn should be called from click event, in other case
    # it will be blocked by chrome policies
    kvm['openPartnerSearch'] = (field) ->
      # serialize current case and service
      srvId = "#{kvm._meta.model.name}:#{kvm.id()}"
      srv  =
        id: srvId
        data: kvm._meta.q.toRawObj()
      kase =
        id: "case:#{kvm._parent.id()}"
        data: kvm._parent._meta.q.toRawObj()

      localStorage[pSearch.storeKey] =
        JSON.stringify {case: kase, service: srv, field: field}
      pSearch.open('case')

  serviceColor: (model, kvm) ->
    # do not run this hook on search screen
    return if /^search/.test(Finch.navigate())

    kvm._svcColor = ko.computed ->
      svcId = kvm._meta.model.name + ':' + kvm.id()
      if kvm._parent
        svcs  = kvm._parent.services().split(',')
        u.palette[svcs.indexOf(svcId) % u.palette.length]
      else
        u.palette[0]
