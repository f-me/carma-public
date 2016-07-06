define [ "utils"
       , "lib/idents"
       , "model/utils"
       , "screens/partnersSearch"
       ], (u, i, mu, pSearch) ->
  ServiceStatus = i.idents "ServiceStatus"
  ServiceType = i.idents "ServiceType"

  # sync with partner search screen
  openPartnerSearch: (model, kvm) ->
    # do not run this hook on search screen
    return if /^search/.test(Finch.navigate())

    # subscibe partner fields to partnersSearch screen events
    for f in model.fields when f.meta?['group-widget'] == "partner"
      do (f) ->
        btn = {}
        btn.state = ko.observable 'legacy'
        btn.newName = ko.observable null
        btn.text = ko.computed ->
          if btn.state() == 'legacy' then 'Старое название' else 'Новое название'
        btn.value = ko.computed ->
          if btn.state() == 'legacy'
          then kvm[f.name]()
          else btn.newName()
        btn.switch = ->
          btn.state(if btn.state() == 'legacy' then 'current' else 'legacy')
        kvm["#{f.name}Btn"] = btn
        partnerId = kvm["#{f.name}Id"]?()
        if partnerId
          $.ajax
            type: "GET"
            url: "/_/Partner/#{partnerId}"
            success: (p) ->
              if kvm[f.name]() != p.name
                btn.newName(p.name)

        n = pSearch.subName f.name, model.name, kvm.id()
        global.pubSub.sub n, (val) ->
          kvm[f.name](val.name)
          kvm["#{f.name}Id"]?(parseInt val.id)
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
            kvm["dealerDistance"]?(val.distanceFormatted)
          kvm['_parent']['refreshHistory']?()

    # this fn should be called from click event, in other case
    # it will be blocked by chrome policies
    kvm['openPartnerSearch'] = (field) ->
      # serialize current case and service
      srvId = "#{kvm._meta.model.name}:#{kvm.id()}"
      srv  =
        id: srvId
        data: kvm._meta.q.toRawObj()
        sType: kvm.type()
      kase =
        id: "case:#{kvm._parent.id()}"
        data: kvm._parent._meta.q.toRawObj()

      localStorage[pSearch.storeKey] =
        JSON.stringify {case: kase, service: srv, field: field}
      pSearch.open('case')

  serviceColor: (model, kvm) ->
    # do not run this hook on search screen
    return if /^search/.test(Finch.navigate())
    ist = u.newComputedDict("iconizedServiceTypes")
    kvm._meta.model.title = ist.getLab kvm.type()
    kvm._svcColor = ko.computed ->
      svcId = kvm._meta.model.name + ':' + kvm.id()
      if kvm._parent
        svcs  = kvm._parent.services().split(',')
        u.palette[svcs.indexOf(svcId) % u.palette.length]
      else
        u.palette[0]

  updateCaseActions: (model, kvm) ->
    kvm._saveSuccessCb = (k, m, j) ->
      # Redirect to back when a service with a self-assigned order
      # action is canceled. Check if the click has just occured to
      # prevent redirections when the case has just been entered.
      if (kvm.buttons.cancel.redirect &&
          (kvm.status?() == ServiceStatus.canceled))
        window.location.hash = "back"
    # Update actions list when new actions might appear
    #
    # TODO The server should notify the client about new actions
    # appearing in the case instead of explicit subscription
    kvm["statusSync"]?.subscribe (nv) ->
      if !nv
        kvm._parent?['renderActions']?()
    kvm["clientSatisfiedSync"]?.subscribe (nv) ->
      if !nv
        kvm._parent?['renderActions']?()

  consultantOperator: (model, kvm) ->
    consType = window.global.idents("ConsultationType")
    role = window.global.idents("Role")
    kvm.consType?.subscribe (v) ->
      if v == consType.oper
        u = window.global.dictionaries.users.byId[kvm.creator()]
        if u and _.contains u.roles, role.consultant_op
          kvm.consultant u.id

  partnerWarnedInTimeBtn: (model, kvm) ->
    # do not run this hook on search screen
    return if /^search/.test(Finch.navigate())
    return if kvm.type() not in
      [ServiceType.tech,
       ServiceType.towage,
       ServiceType.rent,
       ServiceType.taxi,
       ServiceType.sober,
       ServiceType.adjuster]
    kvm.partnerWarnedInTimeBtn =
        click: ->
          $.ajax
            type: "GET"
            url: "/_/ProcessingConfig/1"
            success: (cfg) ->
              dt = 60 * (kvm.suburbanMilage() - kvm.totalMilage()) / cfg.avgSuburbanSpeed
              t1 = moment().add(moment.duration(dt, 'minutes'))
              t2 = moment(kvm.times_expectedServiceStart(), 'DD.MM.YYYY HH:mm:ss')
              kvm.partnerWarnedInTime(t1.format() < t2.format())
        tooltip: ko.computed ->
          if !kvm.suburbanMilage() or !kvm.totalMilage() or kvm.suburbanMilageRegexp() or kvm.totalMilageRegexp()
          then 'Не заполнены поля "Километраж по тахометру" и "Пробег за городом"'
          else ''
        disabled: ko.computed ->
          !kvm.suburbanMilage() or !kvm.totalMilage() or kvm.suburbanMilageRegexp() or kvm.totalMilageRegexp()
