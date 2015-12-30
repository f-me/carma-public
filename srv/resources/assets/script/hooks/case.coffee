define [ "utils"
       , "model/utils"
       , "model/main"
       , "sync/crud"
       , "dictionaries"], (u, mu, main, sync, d) ->
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
    kvm['histShowCall'] = ko.observable true
    kvm['histToggleActi'] = -> kvm.histShowActi(not kvm.histShowActi())
    kvm['histToggleComm'] = -> kvm.histShowComm(not kvm.histShowComm())
    kvm['histToggleCanc'] = -> kvm.histShowCanc(not kvm.histShowCanc())
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
    knockVM['region'] = ko.computed
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
