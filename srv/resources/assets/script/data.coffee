{Promise} = require "carma/vendor"

mapping = Object.freeze

  cfg: Object.freeze
    dicts: "/cfg/dictionaries"

    m: Object.freeze
      Call:     "/cfg/model/Call"
      Case:     "/cfg/model/Case"
      Contract: "/cfg/model/Contract"
      Service:  "/cfg/model/Service"
      Towage:   "/cfg/model/Towage"
      Usermeta: "/cfg/model/Usermeta"

      v: Object.freeze
        kpi: Object.freeze
          GroupKPI: "/cfg/model/GroupKPI?view=kpi"
          OperKPI:  "/cfg/model/OperKPI?view=kpi"
          StatKPI:  "/cfg/model/StatKPI?view=kpi"
        search: Object.freeze
          Call:     "/cfg/model/Call?view=search"
          Case:     "/cfg/model/Case?view=search"
          Contract: "/cfg/model/Contract?view=search"
          Service:  "/cfg/model/Service?view=search"
          Towage:   "/cfg/model/Towage?view=search"
        searchCase: Object.freeze
          Contract: "/cfg/model/Contract?view=searchCase"

  model: Object.freeze
    user:  "/_whoami"
    users: "/_/Usermeta"

  screens: "/screens"

module.exports.data = data = {} # will be filled during app startup

handleItem = (objPath, url) ->
  fetch(url, credentials: "same-origin")
    .then (response) -> response.json()
    .then (json)     -> [objPath, json]

handleBranch = (objPathPfx, obj) ->
  Object.keys(obj).reduce (resultList, key) ->
    val = obj[key]
    if typeof val is "string"
      resultList.push handleItem(objPathPfx.concat(key), val)
    else
      Array::push.apply resultList, handleBranch(objPathPfx.concat(key), val)
    resultList

  , []

module.exports.request = ->
  promises = handleBranch [], mapping
  Promise.all(promises).then (result) ->

    resultObj = result.reduce (resultObj, [objPath, json]) ->

      branch = objPath.slice(0, -1).reduce (obj, key) ->
        obj[key] = {} unless obj.hasOwnProperty key
        obj[key]
      , resultObj

      branch[objPath[objPath.length - 1]] = json
      resultObj

    , {}

    Object.assign data, resultObj
