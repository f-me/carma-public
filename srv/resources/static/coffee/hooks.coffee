this.hooks = ->
  model:
      "*"    : [stdElCb]
      "case" : [candiboberHook]
  observable:
      "*"    : [regexpKbHook, dictionaryKbHook, filesKbHook, dateTimeHook]
      "case" : [caseDescsKbHook, caseEventsHistoryKbHook]
      "tarifOption": [tarifOptNameDef]
      "partner": [bindRemoveService]
      "partner_service": [bindTitleServiceName]
      "cost_serviceTarifOption": [tarifOptNameDef]
      "rent"  : [partnerOptsHook, srvOptUpd, costsMark]
      "tech"  : [partnerOptsHook, srvOptUpd, costsMark]
      "taxi"  : [partnerOptsHook, srvOptUpd, costsMark]
      "sober" : [partnerOptsHook, srvOptUpd]
      "hotel" : [partnerOptsHook, srvOptUpd, costsMark]
      "towage": [partnerOptsHook, srvOptUpd, costsMark]

dictionaryKbHook = (instance, knockVM) ->
  for n of instance.dictionaryFields
    do (n) ->
      fieldName = instance.dictionaryFields[n]
      dict      = instance.fieldHash[fieldName].meta.dictionaryName
      parent    = instance.fieldHash[fieldName].meta.dictionaryParent
      bounded   = instance.fieldHash[fieldName].meta.bounded

      # Perform label-value transformation
      knockVM[fieldName + "Local"] =
        kb.observable instance,
                      key: fieldName
                      read: (k) ->
                        # Read label by real value
                        val = instance.get(k)
                        global.dictValueCache[dict] || getDictionary(dict)
                        lab = global.dictValueCache[dict][val]
                        return (lab || val)
                      write: (lab) ->
                        # Set real value by label
                        val = global.dictLabelCache[dict][lab]
                        # drop value if can't find one for bounded dict
                        if bounded and not val
                        then  instance.set(fieldName, "")
                        else  instance.set(fieldName, val || lab)
                      ,
                      knockVM

regexpKbHook = (instance, knockVM) ->
  # Set observable with name <fieldName>Regexp for inverse of
  # result of regexp checking for every field with meta.regexp
  # annotation. Observable is True when regexp fails.
  for n of instance.regexpFields
    fieldName = instance.regexpFields[n]
    regexp = instance.fieldHash[fieldName].meta.regexp
    ((f, r) ->
      knockVM[fieldName + "Regexp"] =
            kb.observable instance,
                          key: f
                          read: (k) -> not r.test instance.get(k)
    )(fieldName, new RegExp(global.dictLabelCache["_regexps"][regexp]))

filesKbHook = (instance, knockVM) ->
  for n in instance.filesFields
    u = "/upload"
    d = "/s/fileupload"
    knockVM["#{n}UploadUrl"] = ko.computed
      read: ->
        return unless knockVM['id']
        path = "#{instance.model.name}/#{knockVM['id']()}/#{n}"
        "#{u}/#{path}"
    knockVM["#{n}Info"] = ko.computed
      read: ->
        return unless knockVM['id']
        path = "#{instance.model.name}/#{knockVM['id']()}/#{n}"
        fs = knockVM[n]()
        return [] unless fs
        for i in fs.split(',')
          do (i) ->
            url: "#{d}/#{path}/#{i.trim()}"
            name: i.trim()
            ctrl: "#{u}/#{path}/#{i.trim()}"

# Clear dependant dictionary fields when parent is changed
this.dictionaryHook = (elName) ->
  instance = global.viewsWare[elName].bbInstance
  for n of instance.dictionaryFields
    fieldName = instance.dictionaryFields[n]
    parent    = instance.fieldHash[fieldName].meta.dictionaryParent

    if parent
      ((f) ->
        instance.bind("change:" + parent, (v) -> instance.set(f, ""))
      )(fieldName)

this.dateTimeHook = (i, k) ->
  for n in i.dateTimeFields
    do (n) ->
      k["#{n}DateTime"] = ko.computed
        read :       -> k[n]()
        write: (val) -> if Date.parse(val) then k[n](val) else k[n]("")

this.tarifOptNameDef = (i, k) ->
  k["nameOrDef"] = ko.computed
    read: -> k["optionName"]() or "Тарифная опция…"
