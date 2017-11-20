{ko} = require "carma/vendor"

DISABLED_STATES = [4, 9, 19, 20]

cloneTasks = (tasks) -> (Object.assign {}, x for x in tasks)


class AvarcomTasksViewModel
  constructor: ({selectedTasks, serviceStatus, @onChange}) ->
    @subscriptions = [] # mutable

    @tasks = ko.observableArray cloneTasks selectedTasks()
    @subscriptions.push selectedTasks.subscribe (x) => @tasks cloneTasks x

    @isDisabled = ko.observable serviceStatus() in DISABLED_STATES
    @subscriptions.push serviceStatus.subscribe (x) =>
      @isDisabled x in DISABLED_STATES

  dispose: =>
    do x.dispose for x in @subscriptions

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


module.exports =
  componentName: "avarcom-tasks"

  component:
    template:  require "./template.pug"
    viewModel: AvarcomTasksViewModel
