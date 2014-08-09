define ["render/screen"], (r) ->

  attachTo = (parentUrl) ->
    Finch.route "[#{parentUrl}]/stat", (bind) ->
      require ["screens/kpi/stat"], (scr) ->
        scr.screen =
          name : "kpi-stat"
          views: { "kpi-view": scr }
        r.renderScreen scr, _.extend bind

  attachTo: attachTo
