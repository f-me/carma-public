define [ "render/screen"
       , "screens/partnersSearch"
       , "screens/servicesSearch"
       , "screens/callsSearch"
       , "screens/contractsSearch"
       ], (
         r,
         partnersSearch,
         servicesSearch,
         callsSearch,
         contractsSearch
       ) ->

  {Finch} = require "finchjs/finch.min.js"

  attachTo = (parentUrl) ->

    Finch.route "[#{parentUrl}]/partners/:model", (bind) ->
        partnersSearch.screen =
          name : "partnersSearch"
          views:
            "search-view": partnersSearch
        r.renderScreen partnersSearch, bind

    Finch.route "[#{parentUrl}]/services", (bind) ->
        servicesSearch.screen =
          name : "servicesSearch"
          views:
            "search-view": servicesSearch
        r.renderScreen servicesSearch, bind

    Finch.route "[#{parentUrl}]/calls", (bind) ->
        callsSearch.screen =
          name : "callsSearch"
          views:
            "search-view": callsSearch
        r.renderScreen callsSearch, bind

    Finch.route "[#{parentUrl}]/contracts", (bind) ->
        contractsSearch.screen =
          name : "contractsSearch"
          views:
            "search-view": contractsSearch
        r.renderScreen contractsSearch, bind

  attachTo: attachTo
