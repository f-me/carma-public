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
    kvm['historyItems'] = ko.observableArray()

  # Display daily service stats in central pane when `city` field of
  # case is changed.
  cityStatsHook: (model, knockVM) ->
    cityField = "city"
    u.hideComplex
    knockVM[cityField]?.subscribe (new_city) ->
      if new_city
        $.getJSON "/stats/towAvgTime/" + new_city,
          (r) -> $("#city-towage-average-time").text(u.formatSecToMin(r[0]))
      else
        $("#city-towage-average-time").text ''

  regionHook: (model, knockVM) ->
    knockVM['region'] = ko.computed
      read: ->
        res = ''
        city = knockVM.city?()
        if city
          $.bgetJSON "/regionByCity/#{city}",
            (r) -> res = r.join ','
        res

  vwfakeHook: (model, knockVM) ->
    knockVM['callDateVisible'] = ko.computed ->
      not _.contains global.user.roles, global.idents("Role").vwfake

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
