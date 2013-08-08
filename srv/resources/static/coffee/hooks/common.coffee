define [ "utils"
       , "dictionaries"
       ], (u, d) ->
  distanceQuery = (coord1, coord2) ->
    u.stripWs "/geo/distance/#{coord1}/#{coord2}/"

  # - <field>Local for dictionary fields: reads as label, writes real
  #   value back to Backbone model;
  dictionaryKbHook: (m, kvm) ->
    for f in m.fields when f.type == "dictionary"
      do (f) ->
        fieldName  = f.name
        dictName   = f.meta.dictionaryName
        parent     = f.meta.dictionaryParent
        bounded    = f.meta.bounded
        dictType   = f.meta.dictionaryType

        dict = d.dictFromMeta kvm, f.meta
        # Perform label-value transformation
        kvm["#{fieldName}Local"] =
          ko.computed
            read: ->
              # Read label by real value
              val = kvm[fieldName]()
              lab = dict.getLab(val)
              return (lab || val)
            write: (lab) ->
              # Set real value by label
              return if kvm["#{fieldName}Disabled"]()
              val = dict.getVal(lab)
              # drop value if can't find one for bounded dict
              if bounded and not val
              then  kvm[fieldName]("")
              else  kvm[fieldName](val || lab)

        kvm["#{fieldName}Typeahead"] =
          new ThMenu
            select: (v) ->
              kvm[fieldName](dict.id2val(v))
              kvm[fieldName].valueHasMutated()
            dict  : dict

        # dict.disabled = kvm["#{fieldName}Disabled"]()
        kvm["#{fieldName}Disabled"].subscribe (v) -> dict.disabled = v

  regexpKbHook: (model, kvm) ->
    # Set observable with name <fieldName>Regexp for inverse of
    # result of regexp checking for every field with meta.regexp
    # annotation. Observable is True when regexp fails.
    for f in model.fields when f.meta?.regexp?
      fieldName = f.name
      regexp    = f.meta.regexp
      ((f, r) ->
        kvm["#{f}Regexp"] =
              ko.computed -> not r.test kvm[f]()
      )(fieldName, new RegExp(global.dictLabelCache["_regexps"][regexp]))

  # For a field <name> with type=file, add an extra observable
  # <name>Url with absolute URL to the stored file.
  fileKbHook: (model, kvm) ->
    for f in model.fields when f.type == "file"
      do(f) ->
        n   = f.name
        kvm["#{n}Url"] = ko.computed
          read: ->
            p  = "/s/fileupload/attachment/" + kvm.id()
            fs = encodeURIComponent kvm[n]()
            p + "/" + fs

  # Clear dependant dictionary fields when parent is changed
  # this.dictionaryHook = (elName) ->
  #   instance = global.viewsWare[elName].bbInstance
  #   for n of instance.dictionaryFields
  #     fieldName = instance.dictionaryFields[n]
  #     parent    = instance.fieldHash[fieldName].meta.dictionaryParent

  #     if parent
  #       ((f) ->
  #         instance.bind("change:" + parent, (v) -> instance.set(f, ""))
  #       )(fieldName)

  dateTimeHook: (m, k) ->
    for f in m.fields when f.type == "datetime"
      do (f) ->
        n = f.name
        k["#{n}DateTime"] = ko.computed
          read :       -> k[n]()
          write: (val) -> if Date.parse(val) then k[n](val) else k[n]("")

  tarifOptNameDef: (m, k) ->
    k["nameOrDef"] = ko.computed -> k["optionName"]() or "Тарифная опция…"

  # Update a field with the distance between two coordinates whenever
  # they change
  distHook: (model, kvm) ->
    for f in model.fields when f.meta?.distanceTo1? and f.meta?.distanceTo2?
      do (f) ->
        n = f.name
        m = f.meta

        # Find VMs and fields to watch for coordinates
        d1_meta = u.splitFieldInView m.distanceTo1
        if not d1_meta.view?
          vm1 = kvm
        else
          vm1 = u.findVM d1_meta.view

        d2_meta = u.splitFieldInView m.distanceTo2
        if not d2_meta.view?
          vm2 = kvm
        else
          vm2 = u.findVM d2_meta.view

        # Subscribe to change in either of coordinates
        vm1[d1_meta.field].subscribe (new_coord) ->
          other_coord = vm2[d2_meta.field]()
          if other_coord
            $.get distanceQuery(new_coord, other_coord), (resp) ->
              kvm[n](u.formatDistance(resp).toString())

        vm2[d2_meta.field].subscribe (new_coord) ->
          other_coord = vm1[d1_meta.field]()
          if other_coord
            $.get distanceQuery(new_coord, other_coord), (resp) ->
              kvm[n](u.formatDistance(resp).toString())

  # - <field>Locals for dictionary fields: reads array
  #  of { label: ..., value: ... } objects
  # - <field>Many fields: reads nothing, writes - add value
  # to list of values, but only if there is no such val allready
  # - <field>Remove field value is a function, which recieve
  # value and remove it from list
  dictManyHook: (m, k) ->
    for f in m.fields when f.type == "dictionary-many"
      do (f) ->
        n         = f.name
        dictName  = f.meta.dictionaryName
        parent    = f.meta.dictionaryParent
        bounded   = f.meta.bounded
        dictType  = f.meta.dictionaryType

        dict = d.dictFromMeta k, f.meta

        k["#{n}Many"] = ko.computed
          # we don't need any value here
          # I have to retrieve something, to make ko refresh view
          read: -> k[n](); return ""

          write: (lab) ->
            return if lab == ""
            return if k["#{n}Disabled"]()
            val = dict.getVal(lab)
            c = u.splitVals k[n]()
            return if _.contains c, val
            c.push (val or lab)
            if (bounded and val) or (not bounded)
              k[n](c.sort().join(','))

        k["#{n}Locals"] = ko.computed
          read: ->
            for val in u.splitVals k[n]()
              do (val) ->
                lab = dict.getLab(val)
                {label: lab || val, value: val}

        k["#{n}Remove"] = (el) ->
          return if k["#{n}Disabled"]()
          # FIXME: I think, this should be made with bb observable
          # arrays, so we can make them in metamodel and use normal
          # collections, without splitting it manually
          c = u.splitVals(k[n]())
          k[n] _.without(c, el.value).join(',')

        k["#{n}Typeahead"] =
          new ThMenu
            select: (v) ->
              # FIXME: find more appropriate way to set values here
              k["#{n}Many"](dict.getLab(dict.id2val(v)))
            dict  : dict

        dict.disabled = k["#{n}Disabled"]()
        k["#{n}Disabled"].subscribe (v) -> dict.disabled = v

  # For every field {n} with type=json and json-schema=dict-objects,
  # create a new observable {n}Objects, which is an observable array
  # of submodels bound to JSON objects stored in this field in an
  # array.
  #
  # Every object/submodel have fields/writable observables:
  #
  # - "value" which is data stored in that field
  #
  # - "key" is a value of an entry from the dictionary specified in
  # json-dict meta. Key is associated with data.
  #
  # - "note", an arbitary text annotation.
  #
  # Key dictionary entries are available through {n}KeyDictionary
  # observable.
  jsonDictObjsHook: (model, kvm) ->
    for f in model.fields when f.type == "json" &&
                               f.meta?["jsonSchema"] == "dict-objects"
      do (f) ->
        n      = f.name
        nP     = "#{n}Objects"
        # Dictionary used to tag objects in the field
        dict   = d.dictFromMeta kvm, f.meta
        # Regular expression used to check "value" part of every
        # object in the field
        regexp = f.meta.regexp

        # Set to true when sub-vms from {n}Objects update parent field
        # observable. Breaks dependency loops. Has to be false by
        # default to allow parent field changes from outside.
        noloop = false

        # Given a JS object and its index in the underlying json
        # field, return a corresponding item for the {n}Objects array.
        # client argument is true if new object is added from client
        objItem = (obj, i, client) ->
          if client?
            # Placeholder in field contents
            if kvm[n]()? && kvm[n]().length > 0
              full = kvm[n]()
            else
              full = []
            full[i] = obj
            noloop = true
            kvm[n] full
            noloop = false

          # An observable bound to a field in a JSON object
          subfieldObservable = (sf) ->
            # Initial value
            kob = ko.observable obj[sf]

            kob.subscribe (val) ->
              full = kvm[n]()
              full[i][sf] = val
              noloop = true
              kvm[n] full
              noloop = false
            kob

          value: subfieldObservable "value"
          key: subfieldObservable "key"
          note: subfieldObservable "note"
          idx: ko.observable i

          # Derived from dictionaryKbHook. Maps value of key to
          # corresponding label of the widget dictionary.
          keyLocal: ko.computed
            read: ->
              # Read label by real value
              val = if kvm[nP]()[i] then kvm[nP]()[i].key() else ""
              lab = dict.getLab(val)
              return (lab || val)

          regexp: ko.computed ->
            if regexp? && kvm[nP]()[i]
              r = new RegExp global.dictLabelCache["_regexps"][regexp]
              not r.test kvm[nP]()[i].value()
            else
              false

        kvm[nP] = ko.observableArray()
        # Build dependent sub-vms from parent field value
        kvm[n].subscribe (newValue) ->
          if not noloop
            kvm[nP].removeAll()
            if newValue?.length > 0
              for i in [0...newValue.length]
                kvm[nP].push objItem newValue[i], i

        # Add new empty object provided an entry from the associated
        # dictionary
        kvm["#{n}AddObj"] =
          (v) ->
            i = kvm[nP]().length || 0
            obj = key: v.value
            kvm[nP].push objItem obj, i, true

        # Delete an object by its index
        kvm["#{n}DeleteObj"] =
          (v) ->
            return unless confirm "Вы уверены, что хотите удалить запись #{v.value()}?"
            # Remove key by index from field JSON
            newFull = kvm[n]()
            newFull.splice v.idx(), 1
            # Rebuild {n}Objects afterwards
            kvm[n] newFull

        kvm["#{n}KeyDictionary"] =
          ko.observable global.dictionaries[f.meta.dictionaryName].entries

        # Populate {n}Objects with initial values
        kvm[n].valueHasMutated()

  # Standard element callback which will scroll model into view and
  # focus on first field
  stdElCb: (elName) ->
    e = $el(elName)
    # Scroll group to the top of the screen
    if e.hasClass("accordion-inner")
      e.parents(".accordion-group")[0].scrollIntoView()
    f = e.find(".focusable")[0]
    f and f.focus()
