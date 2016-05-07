define [ "utils"
       , "lib/idents"
       , "model/utils"
       , "model/main"
       , "sync/crud"
       , "dictionaries"
       , "components/partnerDelayDialog"],
       (u, idents, mu, main, sync, d, PartnerDelayDialog) ->
  ServiceStatus = idents.idents "ServiceStatus"
  ServiceType = idents.idents "ServiceType"
  ActionType = idents.idents "ActionType"
  ActionResult = idents.idents "ActionResult"
  Program = idents.idents "Program"

  serviceButtons = (kvm) ->
    kvm.status.subscribe ->
      # we need to be sure that udpate is called after applying changes
      # to the server
      kvm._meta.q.save (-> global.Usermeta?.updateAbandonedServices())

    kvm.buttons = {}
    kase = kvm._parent
    sDict = u.newModelDict("ServiceStatus")


    kvm.topLevelButtons = [
      {
        type: 'danger'
        text: sDict.getLab ServiceStatus.mistake
        visible: ko.computed(->
          kvm['status']() == ServiceStatus.creating)
        click: ->
          event.stopPropagation()
          if confirm "Закрыть услугу как ошибочную?"
            kvm['status'] ServiceStatus.mistake
      },
      {
        type: 'warning'
        text: "Обработать позже"
        visible: ko.computed(->
          kvm['status']() == ServiceStatus.creating)
        click: ->
          event.stopPropagation()
          kvm['status'] ServiceStatus.suspended
      },
      {
        type: 'success'
        text: "Активировать"
        visible: ko.computed(->
          kvm['status']() == ServiceStatus.suspended)
        click: ->
          event.stopPropagation()
          kvm['status'] ServiceStatus.creating
      }
    ]

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
      sDict.getLab ServiceStatus.backoffice
    kvm.buttons.backoffice.visible = ko.computed ->
      kvm['status']() == ServiceStatus.creating
    kvm.buttons.backoffice.disabled = ko.computed ->
      u.someEmpty kase, boFlds
    kvm.buttons.backoffice.click = ->
      kvm['status'] ServiceStatus.backoffice

    kvm.buttons.needMakerApproval = {}
    kvm.buttons.needMakerApproval.text =
      sDict.getLab ServiceStatus.makerApproval
    kvm.buttons.needMakerApproval.visible = ko.computed ->
      tgtStatuses = [ ServiceStatus.creating
                    , ServiceStatus.backoffice
                    , ServiceStatus.needPartner
                    ]
      _.contains tgtStatuses, kvm['status']()
    kvm.buttons.needMakerApproval.click = ->
      if confirm "Согласовать оказание услуги с производителем?"
        kvm['status'] ServiceStatus.makerApproval

    kvm.buttons.recallClient = {}
    kvm.buttons.recallClient.text =
      sDict.getLab ServiceStatus.recallClient
    kvm.buttons.recallClient.visible = ko.computed ->
      tgtStatuses = [ ServiceStatus.ordered
                    , ServiceStatus.inProgress
                    , ServiceStatus.needPartner
                    , ServiceStatus.makerApproval
                    ]
      _.contains tgtStatuses, kvm['status']()
    kvm.buttons.recallClient.click = ->
      if confirm "Сообщить клиенту время оказания услуги?"
        kvm['status'] ServiceStatus.recallClient


    # There's no guarantee who renders first (services or actions),
    # try to set up an observable from here
    if not kase['actionsList']?
      kase['actionsList'] = ko.observableArray()

    # Required fields for the cancel button to be enabled
    cnFields = ['clientCancelReason']
    kvm.buttons.cancel = {}
    kvm.buttons.cancel.tooltip = u.reqFieldsTooltip kvm, cnFields
    kvm.buttons.cancel.text =
      sDict.getLab ServiceStatus.canceled
    kvm.buttons.cancel.visible = ko.computed ->
      # Always show in one of these statuses
      tgtStatuses = [ ServiceStatus.creating
                    , ServiceStatus.ordered
                    , ServiceStatus.inProgress
                    , ServiceStatus.needPartner
                    , ServiceStatus.makerApproval
                    ]
      statusOk = (_.contains tgtStatuses, kvm['status']())

      # Show actions for a service in backoffice status only if its
      # order actions are unassigned or assigned to the current user
      ordersUnassigned = false
      myOrder = false
      if kvm['status']() == ServiceStatus.backoffice
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
        kvm['status'] ServiceStatus.canceled

    kvm.buttons.partnerDelay = {}
    kvm.buttons.partnerDelay.text = 'Партнёр опаздывает'
    kvm.buttons.partnerDelay.tooltip = 'Партнёр не выбран'
    kvm.buttons.partnerDelay.disabled = ko.computed -> !kvm.contractor_partnerId()
    kvm.buttons.partnerDelay.visible = ko.computed ->
      kvm.type() in [ServiceType.tech,
                     ServiceType.towage,
                     ServiceType.rent,
                     ServiceType.taxi,
                     ServiceType.sober,
                     ServiceType.adjuster] and
        kvm.status() in [ServiceStatus.ordered, ServiceStatus.inProgress]
    kvm.buttons.partnerDelay.click = ->
      PartnerDelayDialog.show(kvm)


    isSecondarySvc = (s) ->
        s.status() in [ServiceStatus.creating, ServiceStatus.suspended] and
          s.type() in [ServiceType.tech, ServiceType.towage]
    kvm.buttons.anotherPSA = {}
    kvm.buttons.anotherPSA.text = 'Доп. услуга'
    kvm.buttons.anotherPSA.visible = ko.computed ->
      kvm.status() == ServiceStatus.ordered and
        kvm.type() == ServiceType.tech and
        kase.servicesReference().some(isSecondarySvc)
    kvm.buttons.anotherPSA.click = ->
      chkActions = u.svcActions kvm._parent, kvm, [ActionType.checkStatus]
      for a in chkActions
        a['result'] ActionResult.needAnotherService
        # rerender services and actions
        a._meta.q.save (->
          svcs = kvm._parent.services
          svcs.notifySubscribers svcs()
          global.Usermeta?.updateAbandonedServices()
          kvm._parent.renderActions())

      if chkActions.length == 0
        # There is no checkStatus action, just send secondary service(s) to back
        for s in kase.servicesReference()
          if isSecondarySvc(s)
            s['status'] ServiceStatus.backoffice


  # we initialize service buttons here (not in service hooks)
  # just to have case.serivicesReference ready.
  # this allows to check sibling services while initializing buttons
  serviceButtons: (model, kvm) ->
    return if /^search/.test(Finch.navigate())

    for s in kvm.servicesReference()
      serviceButtons(s)

    kvm.servicesReference.subscribe (svcs) ->
      for s in svcs
        serviceButtons(s)

  descsKbHook: (model, knockVM) ->
    mkServicesDescs = (p, s) ->
      desc = u.getServiceDesc(p, s.type())
      if desc
        description: desc
        title:       s._meta.model.title
    knockVM['servicesDescs'] = ko.computed
      read: ->
        p = parseInt knockVM['program']?()
        s = knockVM['servicesReference']?()
        return [] unless p?
        _.chain(s).map((x) -> mkServicesDescs(p,x)).compact().uniq().value()

  programDesc: (model, knockVM) ->
    knockVM['programDesc'] = ko.computed
      read: ->
        u.getProgramDesc (parseInt knockVM['program']()),
                         (parseInt knockVM['subprogram']?())

  caseHistory: (model, kvm) ->
    kvm['historyFilter'] = ko.observable()
    kvm['histShowActi'] = ko.observable true
    kvm['histShowComm'] = ko.observable true
    kvm['histShowCanc'] = ko.observable true
    kvm['histShowDelay'] = ko.observable true
    kvm['histShowCall'] = ko.observable true
    kvm['histToggleActi'] = -> kvm.histShowActi(not kvm.histShowActi())
    kvm['histToggleComm'] = -> kvm.histShowComm(not kvm.histShowComm())
    kvm['histToggleCanc'] = -> kvm.histShowCanc(not kvm.histShowCanc())
    kvm['histToggleDelay'] = -> kvm.histShowDelay(not kvm.histShowDelay())
    kvm['histToggleCall'] = -> kvm.histShowCall(not kvm.histShowCall())
    kvm['historyItems'] = ko.observableArray()
    kvm['endOfHistory'] = ko.observable true
    kvm['lookBackInHistory'] = ->

  # Display daily service stats in central pane when `caseAddress_city` field of
  # case is changed.
  cityStatsHook: (model, kvm) ->
    kvm.caseAddress_city?.subscribe (new_city) ->
      if new_city
        $.getJSON "/stats/towAvgTime/" + new_city,
          (r) -> $("#city-towage-average-time").text(u.formatSecToMin(r[0]))
      else
        $("#city-towage-average-time").text ''

  regionHook: (model, kvm) ->
    kvm['region'] = ko.computed
      read: ->
        res = ''
        city = kvm.caseAddress_city?() || kvm.city?()
        if city
          $.bgetJSON "/regionByCity/#{city}",
            (r) -> res = r.join ','
        res

  vwfakeHook: (model, knockVM) ->
    knockVM['callDateVisible'] = ko.computed ->
      not _.contains global.user.roles, global.idents("Role").vwfake

  timeZoneHook: (model, kvm) ->
    cityTZ = new d.dicts.ModelDict
      dict: 'City'
      meta: {dictionaryLabel: 'timezone'}
    kvm['cityTimeZone'] = ko.computed -> cityTZ.getLab kvm['caseAddress_city']?()
    cityLabel = new d.dicts.ModelDict dict: 'City'
    kvm['cityLabel'] = ko.computed -> cityLabel.getLab kvm['caseAddress_city']?()

    kvm['_tzType'] = ko.observable('client')
    kvm['_timeZone'] = ko.computed ->
      if kvm['_tzType']() == 'client' then kvm['cityTimeZone']() else null
    kvm['_switchTimeZone'] = ->
      kvm['_tzType'](
        if kvm['_timeZone']() then 'local' else 'client'
      )

  carModelInfoHook: (model, knockVM) ->
    dict = new d.dicts.ModelDict
      dict: 'CarModel'
      meta:
        dictionaryKey: 'id'
        dictionaryLabel: 'info'
    knockVM['car_modelInfo'] = ko.computed ->
      dict.getLab knockVM['car_model']?()

  buttons: (model, kvm) ->
    return if /^search/.test(Finch.navigate())
    kvm.buttons = {}

    # Required fields for the needInfo button to be enabled
    niFlds = [ 'city'
             , 'contact_name'
             , 'contact_phone1'
             , 'customerComment'
             , 'program'
             ]

    kvm.buttons.needInfo = {}
    kvm.buttons.needInfo.tooltip = u.reqFieldsTooltip kvm, niFlds
    kvm.buttons.needInfo.text =
      u.newModelDict("CaseStatus").getLab(
              global.idents("CaseStatus").needInfo)
    kvm.buttons.needInfo.visible = ko.computed ->
      statusOk = kvm['caseStatus']() != global.idents("CaseStatus").needInfo
      statusOk && _.isEmpty(kvm['servicesReference']())
    kvm.buttons.needInfo.click = ->
      kvm['caseStatus'] global.idents("CaseStatus").needInfo
    kvm.buttons.needInfo.disabled = ko.computed ->
      u.someEmpty kvm, niFlds

  hasFiles: (model, knockVM) ->
    knockVM['hasFiles'] = ko.computed ->
      knockVM['filesReference']?().length ||
      _.any(_.map(knockVM['servicesReference']?(),
        (srv) -> (srv['filesReference']?().length > 0)))

  vip: (model, kvm) ->
    kvm['vip'] = ko.computed ->
      _.some [
        kvm['contact_phone1Vip']?() ||
        kvm['contact_phone2Vip']?(),
        kvm['contact_phone3Vip']?(),
        kvm['contact_phone4Vip']?()
        ]

  contract: (model, kvm) ->
    kvm.contract?.subscribe (id) ->
      if not kvm.program() or not kvm.subprogram()
        $.getJSON "/_/Contract/#{id}", (c) ->
          kvm.subprogram(c.subprogram)
          $.getJSON "/_/SubProgram/#{c.subprogram}", (s) ->
            kvm.program(s.parent)
