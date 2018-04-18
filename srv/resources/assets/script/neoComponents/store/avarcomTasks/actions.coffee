###
Part of the "tasks" field implementation for "emergency comissioner" service
(see "case" screen). In russian you see "аварийный коммисар".

This store branch contains data of all tasks you could select in typeahead menu.
###

{Immutable: {Record}} = require "carma/vendor"

{makeActions, catchFailure, fetchGet} =
  require "carma/neoComponents/store/utils"

{AvarcomTasksList} = require "./models"


getAvarcomTasksFlow =

  getAvarcomTasksRequest:
    handler: (action, dispatch) ->
      catchFailure dispatch, actions.getAvarcomTasksFailure, null,
        fetchGet "/_/AvarcomTask"
          .then (x) -> AvarcomTasksList.fromPlain x
          .then (avarcomTasks) ->
            success = actions.getAvarcomTasksSuccess
            dispatch success new success.Payload {avarcomTasks}

  getAvarcomTasksSuccess:
    Payload: Record
      avarcomTasks: new AvarcomTasksList

  getAvarcomTasksFailure: null


actions = makeActions __dirname,
  Object.assign {}, getAvarcomTasksFlow

module.exports = actions
