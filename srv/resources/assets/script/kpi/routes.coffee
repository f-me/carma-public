{Finch} = require "carma/vendor"

r = require "carma/render/screen"

attachTo = (parentUrl) ->
  Finch.route "[#{parentUrl}]/stat", (bind) ->
    scr = require "carma/screens/kpi/stat"
    scr.screen =
      name : "kpi-stat"
      views: { "kpi-view": scr }
    r.renderScreen scr, bind

  Finch.route "[#{parentUrl}]/oper", (bind) ->
    scr = require "carma/screens/kpi/oper"
    scr.screen =
      name : "kpi-oper"
      views: { "kpi-view": scr }
    r.renderScreen scr, bind

  Finch.route "[#{parentUrl}]/group", (bind) ->
    scr = require "carma/screens/kpi/group"
    scr.screen =
      name : "kpi-group"
      views: { "kpi-view": scr }
    r.renderScreen scr, bind

module.exports = {
  attachTo
}
