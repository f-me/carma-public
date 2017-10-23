{Promise} = require "carma/vendor"

mapping = {
  dicts:   "/cfg/dictionaries"
  user:    "/_whoami"
  users:   "/_/Usermeta"

  screens: "/screens"
}

module.exports.data = data = {}

module.exports.request = ->
  promises = Object.keys(mapping).map (key) ->
    fetch(mapping[key])
      .then (response) -> response.json()
      .then (json)     -> data[key] = json

  Promise.all(promises).then -> data
