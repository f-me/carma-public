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
define [ "model/render"
       , "dictionaries/local-dict"
       , "sync/crud"
       ],
       (render, dict, sync) ->
  mainSetup = ( localScreens
                   , localRouter
                   , localDictionaries
                   , hooks
                   , user
                   , pubSub) ->
    Screens = localScreens

    dictCache = dict.buildCache(localDictionaries)

    window.global =
        # «Screen» element which holds all views
        topElement: $el("layout")
        screens: Screens
        router: new localRouter
        dictionaries: localDictionaries
        # Maps labels to values for every dictionary
        dictLabelCache: dictCache.labelCache
        # Maps values to labels
        dictValueCache: dictCache.valueCache
        hooks: hooks
        user: user
        model: do ->
          modelCache = {}
          (name, arg) ->
            url = "/cfg/model/#{name}"
            url = url + "?arg=#{arg}" if arg
            if not modelCache[url]
              $.ajax url,
                async: false
                dataType: 'json'
                success: (m) -> modelCache[url] = m
            modelCache[url]
        activeScreen: null
        pubSub: pubSub
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

    Backbone.history.start({pushState: false})

  # Knockout model builder
  #
  # Sets additional observables in Knockout model:
  #
  # - <field>Not for every required field;
  #
  # - maybeId; («—» if Backbone id is not available yet)
  #
  # - modelTitle;
  buildKVM = (model, options) ->

    {elName, fetched, queue, queueOptions, models} = options

    fields    = model.fields
    required  = (f for f in fields when f.meta?.required)

    # Build kvm with fetched data if have one
    kvm = {}
    kvm["_meta"] = { model: model, cid: _.uniqueId("#{model.name}_") }

    # build observables for real model fields
    kvm[f.name] = ko.observable(null) for f in fields

    # set id only when it wasn't set from from prefetched data
    kvm['id'] = ko.observable(fetched?['id'])

    # set queue if have one, and sync it with backend
    kvm._meta.q = new queue(kvm, model, queueOptions) if queue
    kvm[f.name](fetched[f.name]) for f in fields when fetched?[f.name]

    # Set extra observable for inverse of every required
    # parameters, with name <fieldName>Not
    for f in required
      do (f) ->
        n = f.name
        kvm["#{n}Not"] = ko.computed -> kvm["#{n}Regexp"]?() or not kvm[n]()

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
                k = buildKVM global.model(m[0], queueOptions?.modelArg),
                  fetched: {id: m[1]}
                  queue:   queue
                k.parent = kvm
                k
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
            addRef kvm, f.name, opts, (kvm) -> focusRef(kvm)

    kvm["maybeId"] = ko.computed -> kvm['id']() or "—"

    for f in fields when f.type == "nested-model"
      do (f) ->
        kvm["#{f.name}Nested"] = ko.computed ->
          _.map (_.compact kvm[f.name]()), (fetched) ->
            buildKVM models[f.meta.modelName],
              fetched: fetched
              models: models

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
            mbid = parseInt(kvm["maybeId"]())
            return true if readonly
            dixi = kvm['dixi']?() and not kvm["#{name}DisableDixi"]()
            (not _.isNaN mbid)   and
            (dixi or disabled()) and not
            kvm['disableDixi']()
          write: (a) ->
            disabled(not not a)

    # make dixi button disabled
    # until all required fields are filled
    kvm['disableDixiU'] = ko.computed ->
      return unless kvm['dixi']
      notFlds = (not kvm["#{f.name}Not"]() for f in required)
      isFilled = _.all notFlds, _.identity
      if isFilled
        kvm['dixiDisabled'](false) unless kvm['dixi']()
      else
        kvm['dixiDisabled'](true)

    for f in fields when /interval/.test(f.type)
      do (f) ->
        proxy = { begin: null, end: null }
        updateInterval = ->
          if proxy.begin != null and proxy.end != null
            kvm[f.name]([proxy.begin, proxy.end])
        kvm["#{f.name}Begin"] = ko.computed
          read: -> proxy.begin
          write: (v) ->
            proxy.begin = v
            updateInterval()

        kvm["#{f.name}End"] = ko.computed
          read: -> proxy.end
          write: (v) ->
            proxy.end = v
            updateInterval()


    hooks = queueOptions?.hooks or ['*', model.name]
    applyHooks global.hooks.observable, hooks, model, kvm
    return kvm

  #/ Model functions.

  # Return function which will setup views for that model given its
  # form element name and instance id. Standard Backbone-metamodel
  # renderer is used to generate HTML contents in form view. viewsWare
  # is updated properly after the model loading is finished.
  #
  # Following keys are recognized in options argument:
  #
  # - permEl: string, name of element to render permissions template
  # - for model into;
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
  # After model is set, every hook in global.modelHooks["*"] and
  # global.modelHooks[modelName] is called with model view name as
  # argument.

  # model parameter is used here when we need some customized model
  # maybe with filtered some fields or something
  modelSetup = (modelName, model) ->
    return (elName, args, options) ->
      model = global.model(modelName, options.modelArg) if not model
      [kvm, q] = buildModel(model, args, options, elName)

      depViews = setupView(elName, kvm,  options)

      # Bookkeeping
      global.viewsWare[elName] =
        model           : model
        modelName       : model.name
        knockVM         : kvm
        depViews        : depViews

      # update url here, because only top level models made with modelSetup
      kvm["maybeId"].subscribe -> kvm["updateUrl"]()

      screenName = options.screenName or modelName
      kvm["updateUrl"] = ->
        global.router.navigate "#{screenName}/#{kvm.id()}",
                               { trigger: false }

      hooks = options.hooks or ['*', model.name]
      applyHooks global.hooks.model, hooks, elName
      return kvm

  buildModel = (model, args, options, elName) ->
      kvm = buildKVM model,
        elName: elName
        queue: sync.CrudQueue
        queueOptions: options
        fetched: args
      return [kvm, kvm._meta.q]

  buildNewModel = (modelName, args, options, cb) ->
    model = global.model(modelName, options.modelArg)
    [knockVM, q] = buildModel(model, args, options)
    if _.isFunction cb
      q.save -> cb(model, knockVM)
    else
      q.save()
    return [knockVM, q]

  bindDepViews = (knockVM, parentView, depViews) ->
    for k, v of depViews
      global.viewsWare[v] =
        parentView: parentView
      if _.isArray(v)
        ko.applyBindings(knockVM, el(s)) for s in v
      else
        ko.applyBindings(knockVM, el(v))

  setupView = (elName, knockVM,  options) ->
    tpls = render.getTemplates("reference-template")
    depViews = render.kvm(elName, knockVM,  options)
    # Bind group subforms (note that refs are bound
    # separately)
    bindDepViews(knockVM, elName, depViews)
    # Bind extra views if provided
    ko.applyBindings knockVM, el(v) for k, v of options.slotsee when el(v)

    # Bind the model to Knockout UI
    ko.applyBindings(knockVM, el(elName)) if el(elName)

    knockVM['view'] = elName

    for f in knockVM._meta.model.fields when f.type == 'reference'
      do (f) ->
        pview = $("##{knockVM['view']}")
        refsForest = getrForest(knockVM, f.name)
        $("##{refsForest}").empty()
        knockVM["#{f.name}Reference"].subscribe (newValue) ->
          renderRefs(knockVM, f, tpls, options)
        renderRefs(knockVM, f, tpls, options)
    return depViews

  renderRefs = (knockVM, f, tpls, options) ->
    refsForest = getrForest(knockVM, f.name)
    $("##{refsForest}").empty()
    for r in knockVM["#{f.name}Reference"]()
      refBook = render.mkRefContainer(r, f, refsForest, tpls)
      v = setupView refBook.refView, r,
        permEl: refBook.refView + "-perms"
        groupsForest: options.groupsForest
        slotsee: [refBook.refView + "-link"]
      global.viewsWare[refBook.refView] = {}
      global.viewsWare[refBook.refView].depViews = v

  addRef = (knockVM, field, ref, cb) ->
    field = "#{field}Reference" unless /Reference$/.test(field)
    thisId = knockVM._meta.model.name + ":" + knockVM.id()
    ref.args = _.extend({"parentId":thisId}, ref.args)
    buildNewModel ref.modelName, ref.args, ref.options or {},
      (model, refKVM) ->
        newVal = knockVM[field]().concat refKVM
        knockVM[field](newVal)
        cb(_.last knockVM[field]()) if _.isFunction(cb)

  focusRef = (kvm) ->
    e = $('#' + kvm['view'])
    e.parent().prev()[0].scrollIntoView()
    e.find('input')[0].focus()
    e.find('input').parents(".accordion-body").first().collapse('show')


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
    fs = _.chain(hooks[k] for k in selectors).flatten().compact().value()
    f.apply(this, args) for f in fs

  { setup         : mainSetup
  , modelSetup    : modelSetup
  , buildNewModel : buildNewModel
  , buildKVM      : buildKVM
  , addRef        : addRef
  , focusRef      : focusRef
  }
