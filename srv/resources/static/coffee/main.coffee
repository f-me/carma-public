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
# Dictionaries are used by UI to map predefined keys to readable
# values (see knockBackbone).
#
# modelHooks — a hash with lists of hooks called at the end of
# modelSetup for respective model.
#
# user object is stored in global hash and contains data about
# current user.
this.mainSetup = (localScreens, localRouter, localDictionaries, hooks, user) ->
  Screens = localScreens

  dictLabelCache = {}
  dictValueCache = {}

  # Build caches (TODO: Do this on server some day)
  for d of localDictionaries
    do (d) ->
      dictLabelCache[d] = {}
      dictValueCache[d] = {}
      dict = localDictionaries[d]
      if _.isArray(dict.entries)
        for e of dict.entries
          do (e) ->
            l = dict.entries[e].label
            v = dict.entries[e].value
            dictLabelCache[d][l] = v
            dictValueCache[d][v] = l
      else
        for c of dict.entries
          for e of dict.entries[c]
            l = dict.entries[c][e].label
            v = dict.entries[c][e].value
            if l and v
              dictLabelCache[d][l] = v
              dictValueCache[d][v] = l

  window.global =
      # «Screen» element which holds all views
      topElement: $el("layout")
      screens: Screens
      router: new localRouter
      dictionaries: localDictionaries
      # Maps labels to values for every dictionary
      dictLabelCache: dictLabelCache
      # Maps values to labels
      dictValueCache: dictValueCache
      hooks: hooks
      user: user
      activeScreen: null
      # viewWare is for bookkeeping of views in current screen.
      #
      # Hash keys are DOM tree element IDs associated with the
      # model (view names). Values are hashes which contain the
      # following keys:
      #
      # - model (model definition);
      # - modelName;
      # - mkBackboneModel (Backbone constructor);
      # - bbInstance (Backbone model);
      # - knockVM (Knockout ViewModel bound to view);
      # - depViews (hash with views for every reference/group field).
      #
      # When screen is loaded, viewsWare should generally contain
      # only keys which correspond to that screen views. View
      # renderers maintain their viewsWare.
      viewsWare: {}

  Backbone.history.start({pushState: false})

this.el  = (id) -> document.getElementById(id)
this.$el = (id) -> $(el(id))

# Render top-level screen template (static)
#
# args object is passed further to all view setup functions.
renderScreen = (screenName, args) ->
  forgetScreen()
  screen = global.screens[screenName]
  global.activeScreen = screen

  # Highlight the new item in navbar
  $("li.active").removeClass("active")
  $el(screenName + "-screen-nav").addClass("active")

  tpl = $el(screen.template).html()
  global.topElement.html(tpl)
  # Call setup functions for all views, assuming they will set
  # their viewsWare
  for viewName of screen.views
    do (viewName) ->
      setup = screen.views[viewName]
      if not _.isNull(setup) then setup(viewName, args)

# Remove all content of view and clean up wares.
#
# To setup view back again, call
# screen.views[viewName]($el(viewName), args);
forgetView = (viewName) ->
  vW = global.viewsWare[viewName]
  # View may have not setup any knockVM (static views like search)
  if not _.isUndefined(vW.knockVM) then kb.vmRelease(vW.knockVM)
  vW = {}
  $el(viewName).empty()

# Clean up all views on screen and everything.
forgetScreen = ->
  forgetView(viewName) for viewName of global.viewsWare
  global.topElement.empty()
  global.viewsWare = {}
  global.activeScreen = null

# Backbone-Knockout bridge
#
# Sets additional observables in Knockout model:
#
# - <field>Not for every required field;
#
# - maybeId; («—» if Backbone id is not available yet)
#
# - modelTitle;
#
# - <field>Local for dictionary fields: reads as label, writes real
#   value back to Backbone model;
knockBackbone = (instance, viewName) ->
  knockVM = new kb.ViewModel(instance)

  # Set extra observable for inverse of every required
  # parameters, with name <fieldName>Not
  for f in instance.requiredFields
    knockVM[f + "Not"] =
      kb.observable instance,
                    key: f
                    read: (k) -> not instance.get(k)

  for f in instance.referenceFields
    knockBackbone(i) for i in (instance.get(f) or [])

  knockVM["modelTitle"] = kb.observable instance,
                                        key : "title"
                                        read: (k) -> instance.title
  knockVM["maybeId"] =
    kb.observable instance,
                  key : "id"
                  read: (k) -> if instance.isNew() then "—" else instance.id

  applyHooks global.hooks.observable,
             ['*', instance.modelName],
             instance, knockVM, viewName

  return knockVM

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
# - fetchCb: function to be bound to "change" event of Backbone
#   instance. Use this to update references of parent model when
#   referenced instance views are set up.
#
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
this.modelSetup = (modelName) ->
  return (elName, args, options) ->
    mkBackboneModel = backbonizeModel(this.models, modelName)
    instance = new mkBackboneModel(args)
    knockVM  = knockBackbone(instance, elName)

    # External fetch callback
    instance.bind("change", options.fetchCb) if _.isFunction(options.fetchCb)

    # Wait a bit to populate model fields and bind form
    # elements without PUT-backs to server
    #
    # TODO First POST is still broken somewhy.
    window.setTimeout((-> instance.setupServerSync()), 1000)

    # Bookkeeping
    global.viewsWare[elName] =
      model           : this.models[modelName]
      bbInstance      : instance
      modelName       : modelName
      knockVM         : knockVM
      # bbReferences    : bbReferences

    applyHooks(global.hooks.model, ['*', modelName], elName)
    return global.viewsWare[elName]

bindKnockout = (knockVM, elName) -> ko.applyBindings(knockVM, el(elName))
  # Bind the model to Knockout UI
  # ko.applyBindings(knockVM, el(elName))

  # Bind group subforms (note that refs are bound
  # separately)
bindKnockoutMany = (knockVM, groupViews) ->
  ko.applyBindings(knockVM, el(groupViews[s])) for s of groupViews
  # ko.applyBindings(knockVM, el(groupViews[s])) for s of groupViews

  # # Bind extra views if provided
  # ko.applyBindings(knockVM, el(options.slotsee[s])) for s of options.slotsee

# Model method HTTP access point wrt redson location
this.modelMethod = (modelName, method) -> "/_/#{modelName}/#{method}"

# Save instance loaded in view
saveInstance = (viewName) -> global.viewsWare[viewName].bbInstance.save()

# Load existing model instance
createInstance = (viewName, id) ->
  saveInstance(viewName)
  forgetView(viewName)
  global.activeScreen.views[viewName](viewName, {})

# Load existing model instance
restoreInstance = (viewName, id) ->
  forgetView(viewName)
  global.activeScreen.views[viewName](viewName, {"id": id})

# Remove instance currently loaded in view from storage and render
# that view from scratch (if possible)
removeInstance = (viewName) ->
  global.viewsWare[viewName].knockVM._kb_vm.model.destroy()
  forgetView(viewName)
  setup = global.activeScreen.views[viewName]
  setup(viewName, {}) if not _.isNull(setup)

applyHooks = (hooks, selectors, args...) ->
  fs = _.chain(hooks[k] for k in selectors).flatten().compact().value()
  f.apply(this, args) for f in fs