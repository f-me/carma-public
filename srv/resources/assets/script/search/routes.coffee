{Finch} = require "carma/vendor"
r = require "carma/render/screen"

attachTo = (parentUrl) ->

  Finch.route "[#{parentUrl}]/partners/:model", (bind) ->
    partnersSearch = require "carma/screens/partnersSearch"
    partnersSearch.screen =
      name : "partnersSearch"
      views:
        "search-view": partnersSearch
    r.renderScreen partnersSearch, bind

  Finch.route "[#{parentUrl}]/services", (bind) ->
    servicesSearch = require "carma/screens/servicesSearch"
    servicesSearch.screen =
      name : "servicesSearch"
      views:
        "search-view": servicesSearch
    r.renderScreen servicesSearch, bind

  Finch.route "[#{parentUrl}]/calls", (bind) ->
    callsSearch = require "carma/screens/callsSearch"
    callsSearch.screen =
      name : "callsSearch"
      views:
        "search-view": callsSearch
    r.renderScreen callsSearch, bind

  Finch.route "[#{parentUrl}]/contracts", (bind) ->
    contractsSearch = require "carma/screens/contractsSearch"
    contractsSearch.screen =
      name : "contractsSearch"
      views:
        "search-view": contractsSearch
    r.renderScreen contractsSearch, bind

module.exports = {
  attachTo
}
