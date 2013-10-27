define ["model/main", "render/screen"], (main, render) ->
  # Find view for this element
  elementView = (elt) -> _.last($(elt).parents("[id*=view]"))

  # Save instance loaded in view
  saveInstance = (viewName) -> global.viewsWare[viewName].knockVM._meta.q.save()

  window.saveInstance = saveInstance

  # FIXME: remove this function definition
  # and correct module dependencies
  addReference: main.addRef

  # FIXME: remove this function definition
  # and correct module dependencies
  focusReference: main.focusRef

  removeReference: (knockVM, field, ref) ->
    field = field + 'Reference' unless /Reference$/.test(field)
    ref['parentId']?('')
    knockVM[field] _.without(knockVM[field](), ref)

  # Load existing model instance
  createInstance: (viewName, id) ->
    saveInstance(viewName)
    render.forgetView(viewName)
    global.activeScreen.views[viewName](viewName, {})

  # Load existing model instance
  restoreInstance: (viewName, id) ->
    render.forgetView(viewName)
    global.activeScreen.views[viewName](viewName, {"id": id})

  # Remove instance currently loaded in view from storage and render
  # that view from scratch (if possible)
  removeInstance: (viewName) ->
    global.viewsWare[viewName].knockVM.model().destroy()
    render.forgetView(viewName)
    setup = global.activeScreen.views[viewName]
    setup(viewName, {}) if not _.isNull(setup)

  elementView: elementView

  # Find out which model this element belongs to
  elementModel: (elt) ->
    elementView(elt).id.split("-")[0]

  # Get field object for named model and field
  modelField: (modelName, fieldName) ->
    _.find(
      global.model(modelName).fields,
      (f) -> return f.name == fieldName)

  buildSorters: (model) ->
    sorters = {}
    mkSortFns = (name, fn) ->
      sorters["#{name}SortAsc"]  = fn
      sorters["#{name}SortDesc"] = { fn: fn, reverse: true }

    ignoreType = (type) ->
      _.contains ["reference", "nested-model", "json"], type
    for f in model.fields when not ignoreType(f.type)
      do (f) ->
        if f.type == "dictionary"
          mkSortFns f.name, (k) -> k["#{f.name}Local"]()
        else if f.type == "dictionary-many"
          mkSortFns f.name, (k) -> _.pluck k["#{f.name}Locals"](), 'label'
        else if f.type == "checkbox"
          mkSortFns f.name, (k) -> k[f.name]() == true
        else
          mkSortFns f.name, (k) -> k[f.name]
    return sorters
