define [ "render/screen"
       , "screens/kpi/stat"
       , "screens/kpi/oper"
       , "screens/kpi/group"
       ], (
         r,
         stat,
         oper,
         group
       ) ->

  {Finch} = require "finchjs/finch.min.js"

  attachTo = (parentUrl) ->
    Finch.route "[#{parentUrl}]/stat", (bind) ->
        stat.screen =
          name : "kpi-stat"
          views: { "kpi-view": stat }
        r.renderScreen stat, bind

    Finch.route "[#{parentUrl}]/oper", (bind) ->
        oper.screen =
          name : "kpi-oper"
          views: { "kpi-view": oper }
        r.renderScreen oper, bind

    Finch.route "[#{parentUrl}]/group", (bind) ->
        group.screen =
          name : "kpi-group"
          views: { "kpi-view": group }
        r.renderScreen group, bind

  attachTo: attachTo
