# Render top-level screen template (static)
#
# args object is passed further to all view setup functions.
this.renderScreen = (screenName, args) ->
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
this.forgetScreen = ->
  forgetView(viewName) for viewName of global.viewsWare
  global.topElement.empty()
  global.viewsWare = {}
  global.activeScreen = null


this.renderKnockVm = (elName, knockVM, options) ->
  model     = global.models[knockVM.modelName()]
  instance  = knockVM.model()
  content   = renderFields(model, elName)
  groupTpls = getTemplates("group-template")
  depViews  = {}
  for gName, cont of content
    # Main form & permissions
    if gName == "_"
      $el(elName).html(cont)
      $el(options.permEl).html renderPermissions(model, elName)
    else
      view = mkSubviewName(gName, 0, instance.name, instance.cid)
      depViews[gName] = [view]

      # Subforms for groups
      $el(options.groupsForest).append(
          renderDep { refField: gName, refN: 0, refView  : view },
                    groupTpls)
      # render actual view content in the '.content'
      # children of view block, so we can add
      # custom elements to decorate view
      $el(view).find('.content').html(content[gName])
  defaultGroup = "default-#{instance.model.name}"
  if _.has(groupTpls, defaultGroup)
    depViews["default-group"] = defaultGroup
    $el(options.groupsForest).append(
          renderDep { refField: defaultGroup }, groupTpls)
  if _.isFunction(options.renderRefCb)
    options.renderCb(r, subViewN)
  return depViews

this.mkRefContainer = (ref, field, forest, templates)->
  refBook =
    refN: 0
    refModelName: ref.modelName()
    refId: ref.id()
    refField: field
    refClass: mkSubviewClass(field, ref.model().name, ref.model().cid)
    refView: mkSubviewName(field, 0, ref.model().name, ref.model().cid)

  refView = renderDep(refBook, templates)
  $el(forest).append(refView)
  return refBook

# field templates

# Convert model to forest of HTML form elements with appropriate
# data-bind parameters for Knockout, sectioning contents wrt to group
# fields.
#
# For every model field, an appropriate form widget is picked using
# chooseFieldTemplate and actual value may be then rendered in field
# using Knockout.
#
# Field templates are rendered with field definitions as context.
# Extra context attributes include:
#
# - `readonly` for fields which have canWrite=false in form
# description, so templates can use it.
#
# - `viewName` which is viewName passed into renderFields.
#
# To allow rendering of elements which depend on the name of view
# which will hold the instance (like save/remove instance), viewName
# argument is passed.
#
# MAYBE: We can do this on server as well.
#
# @return Hash where keys are names of first fields in each group
# (section names) and values are string with HTML for respective
# section (group field). Groupless fields (those which belong to main
# group) are rendered into value stored under "_" key (main section).
#
# First field of group and fields which have meta annotation
# `mainToo` are always put in main section as well.
#
# There's no way to fully include group fields in main section except
# giving `mainToo` annotation in each field of group.
this.renderFields = (model, viewName) ->
  templates = getTemplates("field-template")

  contents = {}
  fType = ""
  group = ""
  readonly = false
  mainGroup = "_"
  slices

  # Currently we store the name of «current group» while traversing
  # all model fields. When this name changes, we consider the
  # previous group closed. A better approach would be to include
  # group information in served model.
  #
  # We rely on the fact that fields of groups have `<groupname>_`
  # name prefix.
  currentGroup   = mainGroup
  currentSection = mainGroup

  contents[mainGroup] = ""

  for f in model.fields
    if _.isNull(f.meta) or not f.meta.invisible
      f.readonly = f.meta.readonly if f.meta?

      # Note the difference: `meta.readonly` is
      # client-only annotation to override standard
      # permissions. Plain `readonly` is passed to
      # template context and indicates real permissions
      # for field (wrt role information).
      readonly = f.readonly or not model.canUpdate or not f.canWrite

      # Add extra context prior to rendering
      ctx = {readonly: readonly, viewName: viewName}

      # If group ended, or group spliced for different
      # original field started, we'll put contents to
      # different section.
      slices = /(\w+)_(\w+)/.exec(f.name)
      if slices? then group = slices[1] else group = mainGroup

      if f.meta and _.has(f.meta, "infoText")
        f.meta.infoText = global.dictionaries.InfoText[f.meta.infoText]

      if f.type == "dictionary"
        ctx = _.extend ctx,
                       dictionary: global.dictionaries[f.meta.dictionaryName]
      ctx = _.extend(f, ctx)

      # We temprorarily change field type when rendering
      # first field of group, so store real type here.
      realType = f.type

      # Put first field in group in main section, too.
      # Render it as if it had `group` type.
      if group != currentGroup
        currentGroup = group
        if currentGroup == mainGroup
          currentSection = mainGroup
        else
          # Due to prefixing this is unique for any
          # group
          currentSection = f.name

          if f.meta and (not f.meta.mainOnly)
            f.type = "group"
            tpl = chooseFieldTemplate(f, templates)
            contents[mainGroup] += Mustache.render(tpl, ctx)
            f.type = realType

      # Initialiaze new section contents
      if not _.has(contents, currentSection)
          contents[currentSection] = ""

      tpl = chooseFieldTemplate(f, templates)

      # Put field HTML in appropriate section
      contents[currentSection] += Mustache.render(tpl, ctx)

      if f.meta and (f.meta.mainToo or f.meta.mainOnly)
        contents[mainGroup] += Mustache.render(tpl, ctx)
  return contents

# Build name of view for refN-th reference stored in refField of
# instance of model name parentModelName with id parentId.
this.mkSubviewName = (refField, refN, parentModelName, parentId) ->
  "#{parentModelName}-#{parentId}-#{refField}-view-#{refN}"

# Build name of class of views for references stored in refField of
# parentModelName with given id.
this.mkSubviewClass = (refField, parentModelName, parentId) ->
  "#{parentModelName}-#{parentId}-#{refField}-views"

# Pick first template element with id which matches:
# <field.name>-<field.type>, <field.meta.widget>-<field.type>,
# <field.type>
chooseFieldTemplate = (field, templates) ->
  typed_tpl = field.type
  named_tpl = field.name + "-" + field.type
  widget_tpl = ""
  if field.meta? and _.has(field.meta, "widget")
    widget_tpl = field.meta.widget + "-" + field.type

  tpl = pickTemplate(templates,
                           [named_tpl, widget_tpl, typed_tpl, "unknown"])
  return tpl

# Render permissions controls for form holding an instance in given
# view.
#
# @return String with HTML for form
this.renderPermissions = (model, viewName) ->
  modelRo = not model.canUpdate and not model.canCreate and not model.canDelete
  # Add HTML to contents for non-false permissions
  Mustache.render($("#permission-template").text(),
                  _.extend(model, {viewName: viewName, readonly: modelRo}))

# Get all templates with given class, stripping "-<class>" from id of
# every template.
#
# TODO Cache this
this.getTemplates = (cls) ->
  templates = {}
  templates[tmp.id.replace("-" + cls, "")] = tmp.text for tmp in $("." + cls)
  return templates

# Generate HTML contents for view which will be populated by
# referenced instance described by keys of refBook:
#
# refN - number of instance in reference field of parent model;
#
# refId - id of model instance being referenced;
#
# refField - name of field of parent model which stores reference;
#
# refView - name of reference view. where instance will be rendered
# after loading.
#
# refClass - class of reference views in this book.
#
# refBook may contain any other keys as well and will be passed to
# Mustache.render as a context.
#
# Templates will be pickTemplate'd against using
# <refModelName>-<refField>, simply <refField> or default template.
#
# Every view template MUST set div with id=<refView> and
# class=<refClass> where model will be setup; an element with
# id=<refView>-link which will be bound to KnockVM of referenced
# instance; possibly <refView>-perms for rendering instance
# permissions template.
#
# This may also be used to render any dependant views for model to
# maintain unique ids.
this.renderDep = (refBook, templates) ->
  typed_tpl = refBook.refField
  return Mustache.render pickTemplate(templates, [typed_tpl, ""]), refBook

# Pick a template from cache which matches one of given names first.
pickTemplate = (templates, names) ->
  for n in names when _.has(templates, n)
    return templates[n]
  return Mustache.render $("#unknown-template").html(), {names:names}
