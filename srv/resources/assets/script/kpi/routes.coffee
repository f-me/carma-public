define ["render/screen"], (r) ->

  attachTo = (parentUrl) ->
    Finch.route "[#{parentUrl}]/stat", (bind) ->
      require ["screens/kpi/stat"], (scr) ->
        scr.screen =
          name : "kpi-stat"
          views: { "kpi-view": scr }
        r.renderScreen scr, bind

    Finch.route "[#{parentUrl}]/oper", (bind) ->
      require ["screens/kpi/oper"], (scr) ->
        scr.screen =
          name : "kpi-oper"
          views: { "kpi-view": scr }
        r.renderScreen scr, bind

  attachTo: attachTo
