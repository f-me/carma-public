define ["utils"], (u) ->
  nameLocal: (model, knockVM) ->
    knockVM["actionNameLocal"] =
      ko.computed
        read: ->
          actName = (u.newModelDict "ActionType").getLab knockVM.type()
          svcId   = knockVM.serviceId()
          # TODO Speed this up for cases when kvm._parent is present
          if svcId
            $.bgetJSON "/_/Service/#{svcId}", (res) ->
              svcName = (u.newModelDict "ServiceType").getLab res.type
              actName = actName + " (#{svcName})"
          actName

  actionColor: (model, kvm) ->
    kvm._actColor = ko.computed ->
      svcId = kvm.serviceId()
      if kvm._parent
        svcs  = kvm._parent.services().split(',')
        u.palette[svcs.indexOf(svcId) % u.palette.length]
      else
        u.palette[0]
