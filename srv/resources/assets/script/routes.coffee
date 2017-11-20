{_, Finch} = require "carma/vendor"

r      = require "carma/render/screen"
Search = require "carma/search/routes"
KPI    = require "carma/kpi/routes"

# wrapper which will abort execution when user on a brake
addRoute = (url, fn) ->
  Finch.route url,
    setup: (bind) ->
      if _.contains(['rest', 'serviceBreak', 'na'], Finch.navigate())
        return Finch.abort()
      fn(bind)

addRoute "back", (bind) ->
  bo = require "carma/screens/backoffice"
  bo.screen =
    name : "back"
    template: "back-screen-template"
    views:
      "back-form": bo
  r.renderScreen bo, bind

addRoute "call/:id", (bind) ->
  call = require "carma/screens/call"
  call.screen =
    name : "Call"
    template: "call-screen-template"
    views:
      "call-view": call
  r.renderScreen call, bind

addRoute "case/:id/:svc", (bind) ->
  kase = require "carma/screens/case"
  kase.screen =
    name : "case"
    template: "case-screen-template"
    views:
      "case-form": kase
  r.renderScreen kase, bind

addRoute "dict/:dict/:id", (bind) ->
  dictionaries = require "carma/screens/dictionaries"
  dictionaries.screen =
    name : "dictionaries"
    template: "dictionaries-screen-template"
    views:
      "dictionaries-view": dictionaries
  r.renderScreen dictionaries, bind

addRoute "partner/:id", (bind) ->
  partner = require "carma/screens/partners"
  partner.screen =
    name : "partner"
    template: "partner-screen-template"
    views:
      "Partner-view": partner
  r.renderScreen partner, bind

addRoute "processingConfig", (bind) ->
  procCfg = require "carma/screens/processingConfig"
  procCfg.screen =
    name : "processingConfig"
    views:
      "config-view": procCfg
  r.renderScreen procCfg, bind

addRoute "usermeta/:id", (bind) ->
  user = require "carma/screens/dictionaries"
  user.screen =
    name : "dictionaries"
    template: "dictionaries-screen-template"
    views:
      "dictionaries-view": user

  bind.dict = 45
  r.renderScreen user, bind

addRoute "uploads", (bind) ->
  uploads = require "carma/screens/uploads"
  uploads.screen =
    name : "uploads"
    views:
      "uploads-view": uploads
  r.renderScreen uploads, bind

addRoute "printSrv/:id", (bind) ->
  print = require "carma/screens/printService"
  print.screen =
    name : "printSrv"
    template: "printSrv-screen-template"
    views:
      "print-table": print
  r.renderScreen print, bind

addRoute "rkc", (bind) ->
  rkc = require "carma/screens/rkc"
  rkc.screen =
    name : "rkc"
    template: "rkc-screen-template"
    views:
      "rkc-form": rkc
  r.renderScreen rkc, bind

addRoute "supervisor", (bind) ->
  supervisor = require "carma/screens/supervisor"
  supervisor.screen =
    name : "supervisor"
    template: "supervisor-screen-template"
    views:
      "action-form": supervisor
  r.renderScreen supervisor, bind

addRoute "vin", (bind) ->
  vin = require "carma/screens/vin"
  vin.screen =
    name : "vin"
    views:
      "vin-form": vin
  r.renderScreen vin, bind

addRoute "contract/:sub/:id", (bind) ->
  contract = require "carma/screens/contract"
  contract.screen =
    name : "contract"
    template: "contract-screen-template"
    views:
      "contract-form": contract
  # Do not update screen if we stay in the same subprogram. This
  # prevents screen reloading when table rows are clicked on
  # portal screen.
  unless window.global.previousHash?.match "contract/#{bind.sub}/?"
    window.global.previousHash = window.location.hash
    r.renderScreen contract, bind

addRoute "timeline", (bind) ->
  timeline = require "carma/screens/timeline"
  timeline.screen =
    name : "timeline"
    template: "timeline-screen-template"
    views:
      "timeline-view": timeline
  r.renderScreen timeline, bind

Finch.route "rest", (bind) ->
  scr = require "carma/screens/rest"
  scr.screen =
    name: "rest"
    views:
      'rest-view': scr
  r.renderScreen scr, bind

Finch.route "serviceBreak", (bind) ->
  scr = require "carma/screens/serviceBreak"
  scr.screen =
    name: "serviceBreak"
    views:
      'break-view': scr
  r.renderScreen scr, bind

Finch.route "na", (bind) ->
  scr = require "carma/screens/na"
  scr.screen =
    name: "na"
    views:
      'na-view': scr
  r.renderScreen scr, bind

# TODO FIXME a bug here?
addRoute "search", =>
Search.attachTo("search")

# TODO FIXME a bug here?
addRoute "kpi", =>
KPI.attachTo("kpi")

addRoute "diag/edit", (args) ->
  scr = require "carma/screens/diagTree/editor"
  scr.screen =
    name: "diagTreeEditor"
    views:
      "diagTreeEditor-view": scr
  r.renderScreen scr, args

addRoute "diag/show/:caseId", (args) =>
  scr = require "carma/screens/diagTree/show"
  scr.screen =
    name: "diagTreeShow"
    views:
      "diagTreeShow-view": scr
  r.renderScreen scr, args
