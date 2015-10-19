define ["utils", "finchjs/finch.min.js"], (u, f) ->
  # Pretty action name for accordion header
  nameLocal: (model, knockVM) ->
    uid   = knockVM.assignedTo()
    knockVM["myAction"] = ko.computed ->
      uid == window.global.user.id
    return if not /^case/.test(f.Finch.navigate())
    sDict = u.newModelDict "ServiceType"
    uDict = u.newModelDict "Usermeta", false, dictionaryLabel: 'login'
    knockVM["actionNameLocal"] =
      ko.computed
        read: ->
          actName = (u.newModelDict "ActionType").getLab knockVM.type()
          svcId   = knockVM.serviceId()
          # Try to look into parent model for services list
          if knockVM._parent?
            svc = _.find knockVM._parent['servicesReference']?(),
              (s) -> s.id() == svcId
            svcName = sDict.getLab svc?.type()
          # If not, fetch parent service manually
          if svcId && !svcName?
            $.bgetJSON "/_/Service/#{svcId}", (res) ->
              svcName = sDict.getLab res.type
          # Add service name suffix if managed to find it out
          if svcName?
            actName = actName + " (#{svcName})"
          if uid?
            login = uDict.getLab uid
            "@#{login}<br /> #{actName}"
          else
            actName

  actionColor: (model, kvm) ->
    kvm._actColor = ko.computed ->
      svcId = kvm.serviceId()
      if kvm._parent
        svcs  =
          _.map kvm._parent.services()?.split(','),
            (s) -> parseInt s.split(':')?[1]
        u.palette[svcs.indexOf(svcId) % u.palette.length]
      else
        u.palette[0]

  # Prevent scrolling via stdElCb (observable hooks are called before
  # model hooks, thus it works)
  suppressScroll: (model, kvm) ->
    kvm._meta._noscroll = true
