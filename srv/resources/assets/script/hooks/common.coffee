define [ "utils"
       , "dictionaries"
       ], (u, d) ->
  thmenuInit = (k, fname, dict, setter) ->
    thmenu = []
    k["#{fname}TypeaheadBuilder"] = ->
      m = new ThMenu { select: setter, dict  : dict }
      thmenu.push(m)
      return m

    k["#{fname}TypeaheadBuilder"].destroy = ->
      _.map thmenu, (v) -> v.destructor()
      thmenu = []
    k[fname].typeaheadBuilder = k["#{fname}TypeaheadBuilder"]

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

        kvm[fieldName].local = kvm["#{fieldName}Local"]
        # FIXME: this shouldn't rewrite existing observable
        # should be removed after refactoring of models
        kvm[fieldName].text = kvm[fieldName].local
        # Use builder here, because same field can be in group
        # and in the main section, and we need to have
        # different instances og thMenu for them
        thmenuInit kvm, fieldName, dict, (v) ->
          kvm[fieldName](dict.id2val(v))
          kvm[fieldName].valueHasMutated()

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
        kvm["#{f}Regexp"] = ko.computed ->
          return false if kvm[f]() == "" or _.isNull(kvm[f]())
          not r.test kvm[f]()
      )(fieldName, new RegExp(regexp))

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

  dateTimeHook: (m, k) ->
    for f in m.fields when _.contains ["datetime", "Day", "UTCTime"], f.type
      do (f) ->
        n = f.name
        fmt = 'DD.MM.YYYY HH:mm:ss'
        if f.meta.widget == 'datetime-local'
          k["#{n}DateTime"] = ko.computed
            read: ->
              tz = (k._timeZone || k._parent?._timeZone)?()
              if tz
                stamp = moment(k[n](), fmt)
                if stamp.isValid() then stamp.tz(tz).format(fmt) else ''
              else
                k[n]()
            write: (val) ->
              tz = (k._timeZone || k._parent?._timeZone)?()
              stamp = if tz then moment.tz(val, fmt, tz).local() else moment(val, fmt)
              k[n](if stamp.isValid() then stamp.format(fmt) else '')
          k["#{n}TimeZone"] = ko.computed -> (k._timeZone || k._parent?._timeZone)?()
          k["#{n}CityLabel"] = ko.computed -> (k.cityLabel || k._parent?.cityLabel)?()
          k["#{n}SwitchTimeZone"] = -> (k._switchTimeZone || k._parent?._switchTimeZone)?()
        else
          k["#{n}DateTime"] = ko.computed
            read :       -> k[n]()
            write: (val) -> if Date.parse(val) then k[n](val) else k[n]("")


  listOfTimesHook: (m, k) ->
    for f in m.fields when f.meta.widget == 'list-of-times'
      do (f) ->
        n = f.name
        fmtIn = 'YYYY-MM-DD HH:mm:ss'
        fmtOut = 'DD.MM.YYYY HH:mm:ss'
        k["#{n}List"] = ko.computed ->
          tz = (k._timeZone || k._parent?._timeZone)?()
          for t in (k[n]() || [])
            stamp = moment.utc(t, fmtIn)
            if tz
              if stamp.isValid() then stamp.tz(tz).format(fmtOut) else ''
            else
              if stamp.isValid() then stamp.local().format(fmtOut) else ''

  tarifOptNameDef: (m, k) ->
    k["nameOrDef"] = ko.computed -> k["optionName"]() or "Тарифная опция…"

  # - <field>Locals for dictionary fields: reads array
  #  of { label: ..., value: ... } objects
  # - <field>Many fields: reads nothing, writes - add value
  # to list of values, but only if there is no such val allready
  # - <field>Remove field value is a function, which recieve
  # value and remove it from list
  dictManyHook: (m, k) ->
    isMany = (t) -> t == 'dictionary-many' or /^dictionary-set/.test(t)
    for f in m.fields when isMany f.type
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
            return k["#{n}Many"].notifySubscribers() if _.contains k[n](), val
            if (bounded and val) or (not bounded)
              # Start a new observable array or update the existing
              # one
              if _.isEmpty k[n]()
                k[n] []
              v = k[n]()
              v.push (val or lab)
              k[n] v
            k["#{n}Many"].notifySubscribers()

        k["#{n}Locals"] = ko.computed
          read: ->
            for val in (k[n]() || [])
              do (val) ->
                lab = dict.getLab(val)
                { label: lab || val
                , value: val
                , remove: ->
                  return if k["#{n}Disabled"]();
                  k[n] _.without k[n](), val
                }

        # remove this after refactoring, we shouldn't count on template for
        # getting value
        k["#{n}Remove"] = (el) ->
          return false
          return if k["#{n}Disabled"]()
          v = k[n]()
          k[n] _.without v, el.value

        k[n].many = k["#{n}Many"]
        k[n].locals = k["#{n}Locals"]
        k[n].remove = k["#{n}Remove"]

        thmenuInit k, n, dict, (v) ->
          # FIXME: find more appropriate way to set values here
          k["#{n}Many"](dict.getLab(dict.id2val(v)))

        dict.disabled = k["#{n}Disabled"]()
        k["#{n}Disabled"].subscribe (v) -> dict.disabled = v

  # For every field {n} with type=json and jsonSchema=dict-objects,
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
    for f in model.fields when f.type?.match(/json/i) &&
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

          # An observable bound to an entry in a JSON object. Rebuild
          # the whole parent field value on updates
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
              r = new RegExp regexp
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
                # Using objItem established a bi-directional
                # dependency between parent field value (JSON) and
                # entries in it (edited by client)
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
            return unless \
              confirm "Вы уверены, что хотите удалить запись #{v.value()}?"
            # Remove key by index from field JSON
            newFull = kvm[n]()
            newFull.splice v.idx(), 1
            # Rebuild {n}Objects afterwards
            kvm[n] newFull

        kvm["#{n}KeyDictionary"] =
          ko.observable global.dictionaries[f.meta.dictionaryName].entries

        # Populate {n}Objects with initial values
        if kvm[n]()?
          kvm[n].valueHasMutated()

  # Standard element callback which will scroll model into view and
  # focus on first field
  stdElCb: (elName, kvm) ->
    if kvm._meta?._noscroll
      return
    e = $el(elName)
    # Scroll group to the top of the screen
    if e.hasClass("accordion-inner")
      e.parents(".accordion-group")[0].scrollIntoView()
    f = e.find(".focusable")[0]
    f and f.focus()

  bindRemoveHook: (fieldName) ->
    (model, kvm) ->
      kvm[fieldName].subscribe -> u.bindRemove kvm, fieldName

  vipPhones: (model, kvm) ->
    vips = u.newModelDict("VipNumber", false, {dictionaryLabel: 'number'})
    for f in model.fields when f.type == "phone"
      do(f) ->
        n = f.name
        kvm["#{n}Vip"] = ko.computed ->
          vips.getVal(kvm["#{n}"]())
