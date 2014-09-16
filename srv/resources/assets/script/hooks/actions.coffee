define ["utils"], (u) ->
  nameLocal: (model, knockVM) ->
    sDict = u.newModelDict "ServiceType"
    uDict = u.newModelDict "Usermeta", false, dictionaryLabel: 'login'
    knockVM["actionNameLocal"] =
      ko.computed
        read: ->
          actName = (u.newModelDict "ActionType").getLab knockVM.type()
          svcId   = knockVM.serviceId()
          uid     = knockVM.assignedTo()
          # TODO Speed this up for cases when kvm._parent is present
          if svcId
            $.bgetJSON "/_/Service/#{svcId}", (res) ->
              svcName = sDict.getLab res.type
              actName = actName + " (#{svcName})"
          if uid?
            login = uDict.getLab uid
            actName + " @#{login}"
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
