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
      hash = md5(kvm.parentId())
      ix = parseInt(hash.slice(0,6), 16) % u.palette.length
      u.palette[ix]
