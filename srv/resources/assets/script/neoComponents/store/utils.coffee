{Promise, ReduxActions: {createAction}} = require "carma/vendor"

isActionMapObjValid = (v) ->
  Object.keys(v).every (x) ->
    x in ["handler", "payloadCreator", "metaCreator", "Payload"]

# Helper to make actions creators that could be converted to contextual system
# action name by converting to string.
#
# `pathContext` usually is `__dirname` to make context for system action name.
#
# `actionMap` is key-value object where value could be:
#   `null` or `undefined` -
#     Means just to make an action creator
#     which first argument is `payload` for action.
#     See "redux-actions" docs of `createAction` function.
#   function -
#     Handler that gets two arguments: `action` and `dispatch`.
#     `action` -
#       Is produced by action creator created by `createAction` from
#       "redux-actions".
#     `dispatch` -
#       Is a function that make you able to dispatch actions asynchronously
#       (see docs of "redux-thunk"). This handler could return a promise.
#   object -
#     handler: function (optional) -
#       Is just handler described above.
#       `action` will be transformed by `payloadCreator` and `metaCreator`.
#     payloadCreator: function (optional) -
#       See docs of `createAction` of "redux-actions".
#     metaCreator: function (optional) -
#       See docs of `createAction` of "redux-actions".
#
# Any produced action creater always returns a promise
# to make any dispatching process being consistent.
makeActions = (pathContext, actionMap) ->
  Object.keys(actionMap).reduce (result, name) ->
    actionSysName = "#{pathContext}/#{name}"
      .replace /[A-Z]/g, (match) -> "_#{match.toLowerCase()}"
      .toUpperCase()

    v = actionMap[name]

    result[name] = switch
      when not v?
        creator = createAction actionSysName

        (args...) ->
          action = creator args...
          (dispatch) -> Promise.resolve dispatch action

      when typeof v is "function"
        creator = createAction actionSysName

        (args...) ->
          action = creator args...

          (dispatch, getState) ->
            dispatch action
            x = v action, dispatch, getState
            if x.then? then x else Promise.resolve x

      when typeof v is "object" and isActionMapObjValid v
        creator = createAction actionSysName, v.payloadCreator, v.metaCreator
        Payload = v.Payload ? null

        f = (payload, etc...) ->
          if Payload? and payload not instanceof Payload
            throw new Error "Incorrect payload type"

          action = creator payload, etc...

          unless v.handler?
            (dispatch) -> Promise.resolve dispatch action
          else
            (dispatch, getState) ->
              dispatch action
              x = v.handler action, dispatch, getState
              if x.then? then x else Promise.resolve x

        f.Payload = Payload if Payload?
        f

      else throw new Error "Unexpected type"

    result[name].toString = -> actionSysName
    result
  , {}

# Helper for handling error case
catchFailure = (dispatch, failureAction, promise) ->
  promise.catch (err) -> dispatch(failureAction err).then -> throw err

module.exports = {makeActions, catchFailure}
