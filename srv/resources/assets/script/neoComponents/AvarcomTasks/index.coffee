{ko} = require "carma/vendor"
require "./styles.less"

DISABLED_STATES = [4, 9, 19, 20]
cloneTasks = (tasks) -> (Object.assign {}, x for x in tasks)
cachedAvailableTasks = ko.observableArray()

fetchAvailableTasks = ->
  fetch "/_/AvarcomTask", credentials: "same-origin"
    .then (x) => x.json()
    .then (json) =>
      cachedAvailableTasks \
        ({isChecked: false, id, label} \
          for {id, label, isActive} in json when isActive)

hasMatch = (q, x) ->
  q = q.toLowerCase()
  x = x.toLowerCase()
  (w.trim() for w in q.split /\s+/g).every (w) -> ~ x.indexOf w


class AvarcomTasksViewModel
  constructor: ({selectedTasks, serviceStatus, @onChange}) ->
    @subscriptions = [] # mutable

    @tasks = ko.observableArray cloneTasks selectedTasks()
    do =>
      f = (x) => @tasks cloneTasks x
      @subscriptions.push selectedTasks.subscribe f

    @isDisabled = ko.observable serviceStatus() in DISABLED_STATES
    @subscriptions.push serviceStatus.subscribe (x) =>
      @isDisabled x in DISABLED_STATES

    @selectedTask = ko.observable null
    @subscriptions.push @selectedTask.subscribe (x) =>
      return unless x?
      @addTask x
      setTimeout (=> @selectedTask null), 1

    @availableTasks = ko.observableArray \
      @filterAvailableTasks cachedAvailableTasks()
    do =>
      f = => @availableTasks @filterAvailableTasks cachedAvailableTasks()
      @subscriptions.push cachedAvailableTasks.subscribe f, null, "arrayChange"
      @subscriptions.push @tasks.subscribe f, null, "arrayChange"

    @typeaheadHandlerObservable = ko.observable @typeaheadHandler
    @subscriptions.push @availableTasks.subscribe =>
      # Making new unique function here to force 'typeahead'
      # reinitialize 'availableTasks'.
      @typeaheadHandlerObservable @typeaheadHandler.bind null

    unless cachedAvailableTasks.cached?
      cachedAvailableTasks.cached = true
      do fetchAvailableTasks

  dispose: =>
    do x.dispose for x in @subscriptions

  filterAvailableTasks: (tasks) =>
    selectedIds = (id for {id} in @tasks())
    (Object.assign {}, x for x in tasks when x.id not in selectedIds)

  addTask: (label) =>
    @tasks.push Object.assign {}, do =>
      return x for x in @availableTasks() when x.label is label
    @onChange cloneTasks @tasks()

  removeTask: (model, e) =>
    do e.preventDefault
    return if @isDisabled()
    {id} = model
    [task, idx] = do => return [x, i] for x, i in @tasks() when x.id is id
    @tasks.splice idx, 1
    @onChange cloneTasks @tasks()

  onCheckboxChange: (model, e) =>
    do e.preventDefault
    return if @isDisabled()
    @onChange cloneTasks @tasks()

  typeaheadHandler: (q, cb) =>
    cb (x for {label: x} in @availableTasks() when hasMatch q, x)


module.exports =
  componentName: "avarcom-tasks"

  component:
    template:  require "./template.pug"
    viewModel: AvarcomTasksViewModel
