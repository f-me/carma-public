define ["render/screen"], (r) ->

  attachTo = (parentUrl) ->

    for s in ["front", "order", "control"]
      do (s) ->
        Finch.route "[#{parentUrl}]/stat/#{s}", (bind) ->
          require ["screens/kpi/stat"], (scr) ->
            scr.screen =
              name : "kpi-stat-#{s}"
              views: { "kpi-view": scr }
            r.renderScreen scr, _.extend bind, { model: s }

  attachTo: attachTo
