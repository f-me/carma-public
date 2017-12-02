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
