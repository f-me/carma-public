{ko} = require "carma/vendor"
{simpleFuzzySearch} = require "carma/lib/search"
{store} = require "carma/neoComponents/store"

{getAvarcomTasksRequest} =
  require "carma/neoComponents/store/avarcomTasks/actions"

require "./styles.less"

DISABLED_STATES = [4, 9, 19, 20]
cloneTasks = (tasks) -> (Object.assign {}, x for x in tasks)
storeSelector = -> store.getState().get "avarcomTasks"


class AvarcomTasksViewModel
  constructor: ({selectedTasks, serviceStatus, @onChange}) ->
    @subscriptions = [] # mutable

    # Connector to store
    @appState = ko.observable storeSelector()
    @unsubscribeFromAppState = store.subscribe => @appState storeSelector()

    @isInitiated = ko.pureComputed =>
      @appState().get("isLoaded") or @appState().get("isLoading")

    @isLoading = ko.pureComputed => @appState().get("isLoading")
    @isFailed  = ko.pureComputed => @appState().get("isFailed")

    @loadedAvailableTasks = ko.pureComputed =>
      @appState().get("tasks").onlyAvailable()

    @tasks = ko.observableArray cloneTasks selectedTasks()
    do =>
      f = (x) => @tasks cloneTasks x
      @subscriptions.push selectedTasks.subscribe f

    @isDisabled = ko.observable serviceStatus() in DISABLED_STATES
    @subscriptions.push serviceStatus.subscribe (x) =>
      @isDisabled x in DISABLED_STATES

    @isInputVisible = ko.pureComputed =>
      not (@isDisabled() or @isLoading() or @isFailed())

    @selectedTask = ko.observable null
    @subscriptions.push @selectedTask.subscribe (x) =>
      return unless x?
      @addTask x
      setTimeout (=> @selectedTask null), 1

    @availableTasks = ko.observableArray @getAvailableTasks()
    do =>
      f = => @availableTasks @getAvailableTasks()
      @subscriptions.push @loadedAvailableTasks.subscribe f
      @subscriptions.push @tasks.subscribe f, null, "arrayChange"

    @typeaheadHandlerObservable = ko.observable @typeaheadHandler
    @subscriptions.push @availableTasks.subscribe =>
      # Making new unique function here to force 'typeahead'
      # reinitialize 'availableTasks'.
      @typeaheadHandlerObservable @typeaheadHandler.bind null

    unless @isInitiated()
      store.dispatch getAvarcomTasksRequest()

  dispose: =>
    do @unsubscribeFromAppState
    do x.dispose for x in @subscriptions

  getAvailableTasks: =>
    selectedIds = (id for {id} in @tasks())
    @loadedAvailableTasks()
      .filter (x) -> x.get("id") not in selectedIds
      .toJS()

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
    cb (x for {label: x} in @availableTasks() when simpleFuzzySearch q, x)


module.exports =
  componentName: "avarcom-tasks"

  component:
    template:  require "./template.pug"
    viewModel: AvarcomTasksViewModel
