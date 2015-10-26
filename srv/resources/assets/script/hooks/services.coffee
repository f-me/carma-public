define [ "utils"
       , "model/utils"
       , "screens/partnersSearch"
       ], (u, mu, pSearch) ->

  # sync with partner search screen
  openPartnerSearch: (model, kvm) ->
    # do not run this hook on search screen
    return if /^search/.test(Finch.navigate())

    # subscibe partner fields to partnersSearch screen events
    for f in model.fields when f.meta?['group-widget'] == "partner"
      do (f) ->
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

  buttons: (model, kvm) ->
    return if /^search/.test(Finch.navigate())

    kvm.buttons = {}
    kase = kvm._parent
    sDict = u.newModelDict("ServiceStatus")

    kvm.buttons.mistake = {}
    kvm.buttons.mistake.text =
      sDict.getLab global.idents("ServiceStatus").mistake
    kvm.buttons.mistake.visible = ko.computed ->
      kvm['status']() == global.idents("ServiceStatus").creating
    kvm.buttons.mistake.click = ->
      if confirm "Закрыть услугу как ошибочную?"
        kvm['status'] global.idents("ServiceStatus").mistake

    # Required *case* fields for the backoffice button to be enabled
    boFlds = [ 'city'
             , 'contact_name'
             , 'contact_phone1'
             , 'customerComment'
             , 'program'
             ]
    kvm.buttons.backoffice = {}
    kvm.buttons.backoffice.tooltip = u.reqFieldsTooltip kase, boFlds
    kvm.buttons.backoffice.text =
      sDict.getLab global.idents("ServiceStatus").backoffice
    kvm.buttons.backoffice.visible = ko.computed ->
      kvm['status']() == global.idents("ServiceStatus").creating
    kvm.buttons.backoffice.disabled = ko.computed ->
      u.someEmpty kase, boFlds
    kvm.buttons.backoffice.click = ->
      kvm['status'] global.idents("ServiceStatus").backoffice

    kvm.buttons.needMakerApproval = {}
    kvm.buttons.needMakerApproval.text =
      sDict.getLab global.idents("ServiceStatus").makerApproval
    kvm.buttons.needMakerApproval.visible = ko.computed ->
      tgtStatuses = [ global.idents("ServiceStatus").creating
                    , global.idents("ServiceStatus").backoffice
                    , global.idents("ServiceStatus").needPartner
                    ]
      _.contains tgtStatuses, kvm['status']()
    kvm.buttons.needMakerApproval.click = ->
      if confirm "Согласовать оказание услуги с производителем?"
        kvm['status'] global.idents("ServiceStatus").makerApproval

    kvm.buttons.recallClient = {}
    kvm.buttons.recallClient.text =
      sDict.getLab global.idents("ServiceStatus").recallClient
    kvm.buttons.recallClient.visible = ko.computed ->
      tgtStatuses = [ global.idents("ServiceStatus").ordered
                    , global.idents("ServiceStatus").inProgress
                    , global.idents("ServiceStatus").needPartner
                    , global.idents("ServiceStatus").makerApproval
                    ]
      _.contains tgtStatuses, kvm['status']()
    kvm.buttons.recallClient.click = ->
      if confirm "Сообщить клиенту время оказания услуги?"
        kvm['status'] global.idents("ServiceStatus").recallClient


    # There's no guarantee who renders first (services or actions),
    # try to set up an observable from here
    if not kase['actionsList']?
      kase['actionsList'] = ko.observableArray()

    # Required fields for the cancel button to be enabled
    cnFields = ['clientCancelReason']
    kvm.buttons.cancel = {}
    kvm.buttons.cancel.tooltip = u.reqFieldsTooltip kvm, cnFields
    kvm.buttons.cancel.text =
      sDict.getLab global.idents("ServiceStatus").canceled
    kvm.buttons.cancel.visible = ko.computed ->
      # Always show in one of these statuses
      tgtStatuses = [ global.idents("ServiceStatus").creating
                    , global.idents("ServiceStatus").ordered
                    , global.idents("ServiceStatus").inProgress
                    , global.idents("ServiceStatus").needPartner
                    , global.idents("ServiceStatus").makerApproval
                    ]
      statusOk = (_.contains tgtStatuses, kvm['status']())

      # Show actions for a service in backoffice status only if its
      # order actions are unassigned or assigned to the current user
      ordersUnassigned = false
      myOrder = false
      if kvm['status']() == global.idents("ServiceStatus").backoffice
        myOrder = false
        svcActs = u.svcActions kase, kvm,
          [ global.idents("ActionType").orderService
          , global.idents("ActionType").orderServiceAnalyst
          ]
        ordersUnassigned = !_.isEmpty(svcActs) &&
          _.every svcActs, (a) -> _.isNull a.assignedTo()
        myOrder = _.some svcActs, (a) -> a.assignedTo() == global.user.id
      statusOk || myOrder || ordersUnassigned
    kvm.buttons.cancel.disabled = ko.computed ->
      _.isEmpty kvm['clientCancelReason']?()
    kvm.buttons.cancel.click = ->
      if confirm "Выполнить отказ от услуги?"
        # Redirect to #back if own actions closed
        svcActs = u.svcActions kvm._parent, kvm, null
        if _.some(svcActs, (a) -> a.assignedTo() == global.user.id)
          kvm.buttons.cancel.redirect = true
        kvm['status'] global.idents("ServiceStatus").canceled

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
          (kvm.status?() == global.idents("ServiceStatus").canceled))
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
