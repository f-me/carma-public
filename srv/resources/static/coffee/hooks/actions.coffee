define [], ->
	nameLocal: (instance, knockVM) ->
    if instance.name == "action"
      knockVM["actionNameLocal"] =
        ko.computed
          read: ->
            actName = global.dictValueCache.ActionNames[knockVM.name()]
            svcId   = knockVM.parentId()
            if svcId
              modelName = svcId.split(':')[0]
              svcName = global.models[modelName].title
              actName = actName + " (#{svcName})"
            actName
						