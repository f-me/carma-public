define ["utils"], (u) ->
  nameLocal: (model, knockVM) ->
    knockVM["actionNameLocal"] =
      ko.computed
        read: ->
          actName = global.dictValueCache.ActionNames[knockVM.name()]
          svcId   = knockVM.parentId()
          if svcId
            modelName = svcId.split(':')[0]
            svcName = global.model(modelName).title
            actName = actName + " (#{svcName})"
          actName

  actionColor: (model, kvm) ->
    kvm._actColor = ko.computed ->
      svcId = kvm.parentId()
      if kvm._parent
        svcs  = kvm._parent.services().split(',')
        u.palette[svcs.indexOf(svcId) % u.palette.length]
      else
        u.palette[0]
