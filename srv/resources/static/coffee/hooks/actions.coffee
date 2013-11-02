define ["utils"], (utils) ->
  nameLocal: (model, knockVM) ->
    # Workaround for #1373
    if utils.canReadActions()
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
