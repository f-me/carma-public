define ["render/screen"], (r) ->

  attachTo = (parentUrl) ->

    Finch.route "[#{parentUrl}]/partners/:model", (bind) ->
      require ["screens/partnersSearch"], (partnersSearch) ->
        partnersSearch.screen =
          name : "partnersSearch"
          views:
            "search-view": partnersSearch
        r.renderScreen partnersSearch, bind

    Finch.route "[#{parentUrl}]/services", (bind) ->
      require ["screens/servicesSearch"], (servicesSearch) ->
        servicesSearch.screen =
          name : "servicesSearch"
          views:
            "search-view": servicesSearch
        r.renderScreen servicesSearch, bind

    Finch.route "[#{parentUrl}]/calls", (bind) ->
      require ["screens/callsSearch"], (callsSearch) ->
        callsSearch.screen =
          name : "callsSearch"
          views:
            "search-view": callsSearch
        r.renderScreen callsSearch, bind

    Finch.route "[#{parentUrl}]/contracts", (bind) ->
      require ["screens/contractsSearch"], (contractsSearch) ->
        contractsSearch.screen =
          name : "contractsSearch"
          views:
            "search-view": contractsSearch
        r.renderScreen contractsSearch, bind

  attachTo: attachTo
