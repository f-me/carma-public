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
#     Payload: an Immutable's Record (optional) -
#       Constructor for payload data.
#       Keep in mind when you create an action payload is checked that it's
#       an instance of this constructor or throws exception otherwise.
#       See http://facebook.github.io/immutable-js/docs/#/Record
#
# Any produced action creater always returns a promise
# to make any dispatching process being consistent.
makeActions = (pathContext, actionMap) ->
  reducer = (result, name) ->
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
            err = new Error "Incorrect payload type"
            console.error err, "payload:", payload
            throw err

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

  Object.keys(actionMap).reduce reducer, {}

# Helper for handling error case
catchFailure = (dispatch, failureAction, promise) ->
  promise.catch (err) -> dispatch(failureAction err).then -> throw err

fetchMethod = (method, url, data, customOpts) ->
  reqOpts =
    method      : method
    headers     : {"Content-Type": "application/json"}
    credentials : "same-origin"

  # For GET method it is optional
  reqOpts.body = JSON.stringify data if data?

  if customOpts?
    if customOpts.headers?
      headers = Object.assign reqOpts.headers, customOpts.headers
      Object.assign reqOpts, customOpts, {headers}
    else
      Object.assign reqOpts, customOpts

  promise = fetch(url, reqOpts)

  if customOpts?.originalResponse \
     then promise
     else promise.then (x) -> x.json()

# Fetch, POST method, with credentials, JSON data in/JSON data out
fetchPost = (url, data = {}, customOpts = null) ->
  fetchMethod "POST", url, data, customOpts

# Fetch, GET method, with credentials, JSON data in/JSON data out
fetchGet = (url, customOpts = null) ->
  fetchMethod "GET", url, null, customOpts

# Fetch, PUT method, with credentials, JSON data in/JSON data out
fetchPut = (url, data = {}, customOpts = null) ->
  fetchMethod "PUT", url, data, customOpts

module.exports = {makeActions, catchFailure, fetchPost, fetchGet, fetchPut}
