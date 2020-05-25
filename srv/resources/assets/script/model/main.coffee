#/ Screen layout rendering, loading models, frobnicating foobars.
#
# Screens have top-level template and a number of views.
#
# Each view has setup function which accepts DOM element, screen
# arguments and renders HTML to element, and sets up viewsWare
# value for its view element. View name matches the ID of
# enclosing DOM element ($el(<viewName>) = viewName's DOM).
#
# In case view has no standard setup function, null must be
# specified instead.
#
# Screen rendering is called through router.
#
# modelHooks — a hash with lists of hooks called at the end of
# modelSetup for respective model.
#
# user object is stored in global hash and contains data about
# current user.
{$, _, ko, Finch} = require "carma/vendor"

require "carma/lib/serialize"

dict         = require "carma/dictionaries/local-dict"
render       = require "carma/model/render"
Fs           = require "carma/model/fields"
sync         = require "carma/sync/crud"
Idents       = require "carma/lib/idents"
{Config}     = require "carma/lib/config"
{ajaxUpload} = require "carma/lib/upload"

mainSetup = (localDictionaries, hooks, user, pubSub) ->

  dictCache = dict.buildCache localDictionaries
  configmgr = new Config

  # TODO FIXME this should never exists
  window.global = {
    topElement: $el("layout")
    dictionaries: localDictionaries
    # Maps labels to values for every dictionary
    dictLabelCache: dictCache.labelCache
    # Maps values to labels
    dictValueCache: dictCache.valueCache
    hooks
    user
    # Provided a model name, return available idents for that
    # model
    idents: Idents.idents
    # Return client config option value
    config: (cn) -> configmgr.getOption cn
    model: do ->
      modelCache = {}
      (name, view) ->
        url = "/cfg/model/#{name}"
        url += "?view=#{view}" if view
        unless modelCache[url]?
          $.ajax url,
            async: false # TODO FIXME huge design mistake
            dataType: 'json'
            success: (m) -> modelCache[url] = m
        modelCache[url]
    activeScreen: null
    pubSub
    # viewsWare is for bookkeeping of views in current screen.
    #
    # Hash keys are DOM tree element IDs associated with the
    # model (view names). Values are hashes which contain the
    # following keys:
    #
    # - model (model definition);
    #
    # - modelName;
    #
    # - knockVM (Knockout ViewModel bound to view);
    #
    # - depViews (hash with views for every reference/group field).
    #
    # - parentView (name of view for which this view is listed as
    #   dependant (only for group fields; group fields have none of
    #   other values in their viewsWare entry))
    #
    # When screen is loaded, viewsWare should generally contain
    # only keys which correspond to that screen views. View
    # renderers maintain their viewsWare.
    viewsWare: {}
  }

urlFor = ->
  switch @kvm._meta.model.name
    when "Case"
      "/#case/#{this()}"
    else
      "/##{@kvm._meta.model.name}/#{this()}"

# `path` is an array of keys sequence of a branch you're getting.
# `defaultValue` is used only for `undefined`
# (not for `null` or other "falsy" value).
safeGetProp = (obj, path, defaultValue=undefined) ->
  if obj is null or typeof obj isnt 'object'
    return defaultValue

  result = obj

  for propName, i in path
    x = result[propName]
    if i isnt (path.length - 1) and (x is null or typeof x isnt 'object')
      result = undefined
      break
    else
      result = x

  if result is undefined then defaultValue else result

# Knockout model builder
#
# Sets additional observables in Knockout model:
#
# - <field>Not for every required field;
#
# - maybeId; («—» if Backbone id is not available yet)
#
# - modelTitle;
buildKVM = (model, options = {}) ->
  {fields} = model
  required = (f for f in fields when f.meta?.required)

  cannotModifyModelData =
    not model.canUpdate and not model.canCreate and not model.canDelete

  # Build kvm with fetched data if have one
  kvm = {}
  kvm._parent = options.parent
  kvm._saveSuccessCb = options.saveSuccessCb
  {fetched, queue, queueOptions, models} = options
  kvm.safelyGet = (prop) -> kvm[prop]?() or ''
  kvm._meta = {model, cid: _.uniqueId("#{model.name}_"), cannotModifyModelData}

  kvm.safeGetProp = safeGetProp

  # build observables for real model fields
  for f in fields
    do (f) ->
      kvm[f.name] = ko.observable null
      kvm[f.name].field = f
      kvm[f.name].kvm   = kvm

  # set id only when it wasn't set from from prefetched data
  # FIXME: remove this, id should be created from fields of model
  # when we have it there
  unless _.isFunction kvm['id']
    kvm['id'] = ko.observable()
    kvm['id'].kvm = kvm
  kvm['id'] fetched['id'] unless _.isUndefined fetched?['id']

  kvm["id"].urlFor = urlFor

  # set queue if have one, and sync it with backend
  kvm._meta.q = new queue kvm, model, queueOptions if queue
  kvm[f.name] fetched[f.name] for f in fields when fetched?[f.name]

  for f in fields
    do (f, n = f.name) ->

      # Set extra observable for inverse of every required
      # parameters, with name <fieldName>Not
      # required can be customized with function put into
      # kvm[<fieldName>].customRequired
      kvm[n].customRequired = ko.observable()
      kvm["#{n}Not"] = ko.computed ->
        realNot = kvm["#{n}Regexp"]?() or not kvm[n]()
        deflt   = f.meta.required and realNot
        custom  = kvm[n].customRequired()
        if _.isFunction custom then custom(deflt, realNot) else deflt

      # Extra observable to control visibility of the field,
      # customizations work same as required
      kvm[n].customVisible = ko.observable()
      kvm["#{n}Visible"] = ko.computed ->
        if _.isFunction kvm[n].customVisible()
          kvm[n].customVisible()()
        else if f.meta.visibleIf
          for p of f.meta.visibleIf
            val = kvm[p]()
            if f.meta.visibleIf[p].some((x) -> String(x) is String(val))
              return true
          return false
        else
          true

      # Determines the sync state with the server
      kvm["#{n}Sync"] = ko.observable false
      kvm["#{n}InvalidDate"] = ko.observable false

      # Read image from file or clipboard as base64 and store it to kvm[n]
      if f.meta?.widget is "image-uploader"
        # This field is displayed to the user instead of the real one.
        kvm["#{n}FakeText"] = ko.observable ""

        # Read image as data URL (base64) and store to the real field.
        readImage = (data) ->
          reader = new FileReader()
          reader.onload = (ev) -> kvm[n] ev.target.result
          reader.readAsDataURL data

        # Handle file load via <input type=file>.
        kvm["#{n}UploadHandler"] = (el) ->
          kvm["#{n}FakeText"] el.files[0].name
          readImage el.files[0]

        # Handle file pasting from clipboard.
        pasteHandler = (ev) ->
          data = ev.clipboardData  || ev.originalEvent.clipboardData
          for i in [0..data.items.length] # NB. can't use iterator here
            it = data.items[i]
            if it and it.kind == "file" and it.type.startsWith("image/")
              readImage it.getAsFile()
              break

        kvm["#{n}OnFocus"] = ->
          window.addEventListener "paste", pasteHandler, false
        kvm["#{n}OnBlur"] = ->
          window.removeEventListener "paste", pasteHandler, false

        kvm["#{n}Warning"] = ko.computed ->
          if kvm[n]()?.length > 20*1024
            return "Размер изображения превышает 20 Кб, это очень плохо."

      # Handler for clicking on "upload" button
      if f.meta?.widget is "inline-uploader"

        kvm["#{n}IsFormVisible"] = ko.pureComputed ->
          isEmpty = _.isEmpty kvm[n]() # it's important to subscribe to it first
          not f.meta["single-uploader"] or isEmpty

        kvm["#{n}IsProcessing"] = ko.observable no

        kvm["#{n}UploadHandler"] = (files) ->
          return if files.length is 0
          kvm["#{n}IsProcessing"] yes

          ajaxUpload "/upload/#{model.name}/#{kvm["maybeId"]()}/#{n}", files[0]
            # Re-read instance data when a new attachment is added
            .always -> kvm["#{n}IsProcessing"] no
            .done   -> do kvm._meta.q.fetch
            .fail   -> window.alert "Не удалось загрузить файл!"

        kvm["#{n}DetachFile"] = (vm, e) ->
          return unless confirm "Вы уверены, что хотите открепить этот файл?"
          # Cut out attachment ref and re-save the instance
          ref = "Attachment:#{vm.id()}"
          kvm[n] _.without(kvm[n]().split(','), ref).join ','
          do kvm._meta.q.save

      # special observable for text, so it won't be saved on update null -> ""
      # #1221
      # This cycle build presentation computed, which can be safely binded
      # to ui elements
      do ->
        fn = read: null, write: null
        switch f.type
          when "Double"
            fn =
              read: ->
                return kvm[n]() unless _.isNumber kvm[n]()
                val = kvm[n]().toFixed 3
                if val.search(/\./) < 0 then val else val.replace(/\.?0*$/, '')
          when "DiffTime"
            twoDig = (v) -> if v < 10 then "0#{v}" else "#{v}"
            fn =
              read: ->
                d = kvm[n]()
                s = d % 60
                m = Math.floor(d / 60) % 60
                h = Math.floor(d / 60 / 60)
                "#{h}:#{twoDig(m)}:#{twoDig(s)}"
              write: (v) ->
                [h, m, s] = map v.split(":"), parseInt
                kvm[n] s + m*60 + h*3600
        defaults =
          read: ->
            v = kvm[n]()
            if _.isNull(v) or _.isUndefined(v) then "" else v
          write: (v) ->
            return if _.isEmpty(kvm[n]()) and v == ""
            kvm[n] v

        kvm["#{n}Text"] = ko.computed
          read:  fn.read  or defaults.read
          write: fn.write or defaults.write

        kvm[n].text = kvm["#{n}Text"]

  # This is required to initialize timeZone-related observables in
  # case's kvm. We need them to be ready before services initialization.
  hooks = queueOptions?.hooks or ['*', model.name]
  applyHooks window.global.hooks.preinit, hooks, model, kvm

  # Setup reference fields: they will be stored in <name>Reference as array
  # of kvm models
  for f in fields when f.type == "reference"
    do (f) ->
      kvm["#{f.name}Reference"] =
        ko.computed
          read: ->
            # FIXME: this will be evaluated on every write
            # so reference kvms is better be cached or there will be
            # get on every new reference creation
            return [] unless kvm[f.name]()
            rs = kvm[f.name]().split(',')
            return [] unless rs
            ms = (m.split(':') for m in rs)
            for m in ms
              # HACK: have to use this temporary observable, wich is
              # disposed right after creation, so current computed
              # won't subscribe to any field in built kvm #1860
              k = ko.computed ->
                buildKVM window.global.model(m[0], queueOptions?.modelArg),
                  fetched: {id: m[1]}
                  queue:   queue
                  parent:  kvm
              refKVM = k.peek()
              k.dispose()
              refKVM
          write: (v) ->
            ks = ("#{k._meta.model.name}:#{k.id()}" for k in v).join(',')
            kvm[f.name](ks)

      # setup reference add button
      if f.meta?.model
        # define add-reference-button ko.bindingHandlers.bindClick function
        kvm["add#{f.name}"] = ->
          opts =
            modelName: f.meta.model
            options:
              modelArg: queueOptions?.modelArg
              parentField: 'parentId'
              newStyle: false
          addRef kvm, f.name, opts, (kvm) -> focusRef(kvm)

  # Setup new-style reference (IdentList) fields: they will be
  # stored in <name>Reference as array of kvm models.
  #
  # IdentList fields store a list of instance ids (as array of
  # numbers). Thus, meta.model is required when using such fields.
  # The referenced model must have parent field.
  for f in fields when f.type == "IdentList"
    do (f) ->
      kvm["#{f.name}Reference"] =
        ko.computed
          read: ->
            # FIXME: this will be evaluated on every write
            # so reference kvms is better be cached or there will be
            # get on every new reference creation
            return [] unless kvm[f.name]()
            ids = kvm[f.name]()
            return [] unless ids
            for i in ids
              buildKVM window.global.model(f.meta.model, queueOptions?.modelArg),
                fetched: {id: i}
                queue:   queue
                parent:  kvm
          write: (v) ->
            ks = _.map v, (k) -> k.id()
            kvm[f.name](ks)

      # setup reference add button
      if f.meta?.model
        # define add-reference-button ko.bindingHandlers.bindClick function
        kvm["add#{f.name}"] = ->
          opts =
            modelName: f.meta.model
            options:
              modelArg: queueOptions?.modelArg
              newStyle: true
              parentField : 'parent'
          addRef kvm, f.name, opts, (kvm) -> focusRef(kvm)

  kvm["maybeId"] = ko.computed -> kvm['id']?() or "—"

  for f in fields when f.type == "nested-model"
    do (f) ->
      kvm["#{f.name}Nested"] = ko.computed ->
        _.map (_.compact kvm[f.name]()), (fetched) ->
          buildKVM models[f.meta.modelName], {fetched, models}

  # disable dixi filed for model
  kvm['disableDixi'] = ko.observable(false)

  # - <field>Disabled fields: used to make field look like readonly
  for f in fields
    do (f) ->
      name = f.name
      disabled = ko.observable(false)
      readonly = f.meta?.readonly
      kvm["#{name}DisableDixi"] = ko.observable(false)
      kvm["#{name}Disabled"]    = ko.computed
        read: ->
          return true if options.waitFor? and not kvm[options.waitFor]()?
          mbid = parseInt kvm["maybeId"]()
          return true if readonly
          dixi = kvm['dixi']?() and not kvm["#{name}DisableDixi"]()
          (not _.isNaN mbid) \
            and (dixi or disabled()) \
            and not kvm['disableDixi']()
        write: (a) ->
          disabled Boolean a
      kvm[name].disableDixi = kvm["#{name}DisableDixi"]
      kvm[name].disabled    = kvm["#{name}Disabled"]

  # make dixi button disabled
  # until all editable required fields are filled
  kvm['disableDixiU'] = ko.computed ->
    return unless kvm['dixi']
    notFlds = (not kvm["#{f.name}Not"]() \
      for f in required when f.canWrite && f.name != "dixi")
    isFilled = _.all notFlds, _.identity
    if isFilled
      kvm['dixiDisabled'](false) unless kvm['dixi']()
    else
      kvm['dixiDisabled'](true)

  for f in fields when /interval/.test(f.type)
    do (f) -> Fs.interval(kvm[f.name])

  kvm.toJSON = ->
    r = {}
    for f in kvm._meta.model.fields when kvm[f.name]()
      r[f.name] = kvm[f.name]()
    r

  kvm.fromJSON = (obj) ->
    for f in kvm._meta.model.fields when obj[f.name]
      kvm[f.name](obj[f.name])

  hooks = queueOptions?.hooks or ['*', model.name]
  applyHooks window.global.hooks.observable, hooks, model, kvm
  return kvm

# Cleanup stuff that can prevent remove by gc
cleanupKVM = (kvm) =>
  kvm._meta.q?.destructor?()
  for k in kvms.items()
    for n, f of k when ko.isComputed f
      f.dispose()
    for n, f of k when /TypeaheadBuilder$/.test(n)
      f.destroy()


#/ Model functions.

# Return function which will setup views for that model given its
# form element name and instance id. Standard Backbone-metamodel
# renderer is used to generate HTML contents in form view. viewsWare
# is updated properly after the model loading is finished.
#
# Following keys are recognized in options argument:
#
# - slotsee: array of element IDs:
#
#   [foo-title", "overlook"]
#
#   which will be ko.applyBindings'd to with model after it's
#   finished loading, in addition to elName;
#
# FIXME: remove fetchcb info from here or update to actual implemented info
# - fetchCb: function to be bound to "change" event of Backbone
#   instance. Use this to update references of parent model when
#   referenced instance views are set up.
#
# FIXME: is this is still used?
# - refs: Describe what references model has and where to render
#   their views. This key is an array of objects:
#
#   [{
#      field: "foo",
#      forest: "foo-subrefs"
#    },
#    {
#      field: "bar",
#      forest: "main-subref",
#      modelName: "fooReferencedModel"
#    }]
#
#   field sets the field of parent model where references are stored,
#   forest is the name of element to render views for references into.
#
#   Views generated for references are stored in viewsWare, so that
#   parent instance can get access to its reference views:
#
#   > viewsWare["parent-view"].depViews["some-ref-field"]
#   ["view-1", "view-2"]
#
#   etc., where "view-1" and "view-2" were generated for instances
#   which are referenced in "some-ref-field".
#
# - groupsForest: The name of forest where to render views for field
#   groups. Views generated for groups are stored in depViews under
#   viewsWare entry for parent view. Referenced models are
#   recursively rendered with the same value of groupsForest (so
#   parent model and its children share the same groupsForest).
#
# After model is set, every hook in window.global.modelHooks["*"] and
# window.global.modelHooks[modelName] is called with model view name as
# argument.

# model parameter is used here when we need some customized model
# maybe with filtered some fields or something
modelSetup = (modelName, model) -> (elName, args, options) ->
  model = window.global.model modelName, options.modelArg unless model
  [kvm, q] = buildModel model, args, options

  depViews = setupView elName, kvm, options

  # Bookkeeping
  window.global.viewsWare[elName] = {
    model
    modelName: model.name
    knockVM: kvm
    depViews
  }

  # update url here, because only top level models made with modelSetup
  kvm["maybeId"].subscribe -> kvm["updateUrl"]()

  screenName = options.screenName or modelName
  kvm["updateUrl"] = -> Finch.navigate "#{screenName}/#{kvm.id()}", true

  hooks = options.hooks or ['*', model.name]
  applyHooks window.global.hooks.model, hooks, elName, kvm
  kvm

buildModel = (model, args, options) ->
  kvm = buildKVM model,
    queue: sync.CrudQueue
    queueOptions: options
    parent: options.parent
    saveSuccessCb: options.saveSuccessCb
    fetched: args
    waitFor: options.waitFor ? null
  Object.assign kvm, options.extendKVM # optional extending built KVM
  [kvm, kvm._meta.q]

buildNewModel = (modelName, args, options, cb) ->
  model = window.global.model modelName, options.modelArg
  [knockVM, q] = buildModel model, args, options

  if _.isFunction cb
    q.save -> cb model, knockVM
  else
    q.save()

  [knockVM, q]

# little helper, ko 3.1.0 don't allow to bind to already binded dom
# so cleanup it first
rebindko = (kvm, el) =>
  ko.cleanNode el
  ko.applyBindings kvm, el

bindDepViews = (knockVM, parentView, depViews) ->
  for k, v of depViews
    window.global.viewsWare[v] = {parentView}
    if _.isArray v
      rebindko knockVM, el s for s in v
    else
      rebindko knockVM, el v

setupView = (elName, knockVM, options) ->
  knockVM['view'] = elName

  tpls = render.getTemplates "reference-template"
  depViews = render.kvm elName, knockVM, options
  # Bind group subforms (note that refs are bound
  # separately)
  bindDepViews knockVM, elName, depViews
  # Bind extra views if provided
  rebindko knockVM, el v for k, v of options.slotsee when el v

  # Bind the model to Knockout UI
  rebindko knockVM, el elName if el elName

  for f in knockVM._meta.model.fields when f.type in ['reference', 'IdentList']
    do (f) ->
      refsForest = getrForest knockVM, f.name
      $("##{refsForest}").empty()

      knockVM["#{f.name}Reference"].subscribe (newValue) ->
        renderRefs knockVM, f, tpls, options

      renderRefs knockVM, f, tpls, options

  depViews

renderRefs = (knockVM, f, tpls, options) ->
  refsForest = getrForest(knockVM, f.name)
  $("##{refsForest}").empty()
  for r in knockVM["#{f.name}Reference"]()
    refBook = render.mkRefContainer(r, f, refsForest, tpls)
    v = setupView refBook.refView, r,
      groupsForest: options.groupsForest
      slotsee: [refBook.refView + "-link"]
    window.global.viewsWare[refBook.refView] = {}
    window.global.viewsWare[refBook.refView].depViews = v

addRef = (knockVM, field, ref, cb) ->
  field = "#{field}Reference" unless /Reference$/.test(field)
  parentField = ref.options.parentField
  thisId = knockVM.id()
  patch = {}
  patch[parentField] = thisId
  ref.args = _.extend(patch, ref.args)
  buildNewModel ref.modelName, ref.args, ref.options or {},
    (model, refKVM) ->
      newVal = knockVM[field]().concat refKVM
      knockVM[field](newVal)
      cb(_.last knockVM[field]()) if _.isFunction(cb)

focusRef = (kvm) ->
  e = $('#' + kvm['view'])
  e.parent().prev()[0].scrollIntoView()
  e.find('input')[0].focus()
  accordion = e.find('input').parents(".accordion-body").first()
  if !accordion.hasClass('in')
    accordion.collapse('show')

getrForest = (kvm, fld) ->
  modelName = kvm._meta.model.name
  cid       = kvm._meta.cid
  fcid = "#{modelName}-#{cid}-#{fld}-references"
  fold = "#{modelName}-#{fld}-references"
  if $("##{fcid}")[0]
    return fcid
  else
    return fold

applyHooks = (hooks, selectors, args...) ->
  fs = _.map selectors, (s) -> hooks[s]
  f.apply(this, args) for f in _.compact _.flatten fs

module.exports = {
  setup: mainSetup
  modelSetup
  buildNewModel
  buildKVM
  cleanupKVM
  addRef
  focusRef
}
