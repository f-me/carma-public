define [ "render/screen"
       , "search/routes"
       , "kpi/routes"
       , "screens/backoffice"
       , "screens/call"
       , "screens/case"
       , "screens/dictionaries"
       , "screens/partners"
       , "screens/processingConfig"
       , "screens/uploads"
       , "screens/printService"
       , "screens/rkc"
       , "screens/rkcFront"
       , "screens/rkcOps"
       , "screens/supervisor"
       , "screens/vin"
       , "screens/contract"
       , "screens/timeline"
       , "screens/rest"
       , "screens/serviceBreak"
       , "screens/na"
       ], (
         r,
         Search,
         KPI,
         bo,
         call,
         kase,
         dictionaries,
         partner,
         procCfg,
         uploads,
         print,
         rkc,
         rkcFront,
         rkcOps,
         supervisor,
         vin,
         contract,
         timeline,
         rest,
         serviceBreak,
         na
       ) ->

  {Finch} = require "finchjs/finch.min.js"

  # wrapper which will abort execution when user on a brake
  addRoute = (url, fn) ->
    Finch.route url,
      setup: (bind) ->
        if _.contains(['rest', 'serviceBreak', 'na'], Finch.navigate())
          return Finch.abort()
        fn(bind)

  addRoute "back", (bind) ->
      bo.screen =
        name : "back"
        template: "back-screen-template"
        views:
          "back-form": bo
      r.renderScreen bo, bind

  addRoute "call/:id", (bind) ->
      call.screen =
        name : "Call"
        template: "call-screen-template"
        views:
          "call-view": call
      r.renderScreen call, bind

  addRoute "case/:id", (bind) ->
      kase.screen =
        name : "case"
        template: "case-screen-template"
        views:
          "case-form": kase
      r.renderScreen kase, bind

  addRoute "dict/:dict/:id", (bind) ->
      dictionaries.screen =
        name : "dictionaries"
        template: "dictionaries-screen-template"
        views:
          "dictionaries-view": dictionaries
      r.renderScreen dictionaries, bind

  addRoute "partner/:id", (bind) ->
      partner.screen =
        name : "partner"
        template: "partner-screen-template"
        views:
          "Partner-view": partner
      r.renderScreen partner, bind

  addRoute "processingConfig", (bind) ->
      procCfg.screen =
        name : "processingConfig"
        views:
          "config-view": procCfg
      r.renderScreen procCfg, bind

  addRoute "usermeta/:id", (bind) ->
      dictionaries.screen =
        name : "dictionaries"
        template: "dictionaries-screen-template"
        views:
          "dictionaries-view": dictionaries

      bind.dict = 45
      r.renderScreen dictionaries, bind

  addRoute "uploads", (bind) ->
      uploads.screen =
        name : "uploads"
        views:
          "uploads-view": uploads
      r.renderScreen uploads, bind

  addRoute "printSrv/:id", (bind) ->
      print.screen =
        name : "printSrv"
        template: "printSrv-screen-template"
        views:
          "print-table": print
      r.renderScreen print, bind

  addRoute "rkc", (bind) ->
      rkc.screen =
        name : "rkc"
        template: "rkc-screen-template"
        views:
          "rkc-form": rkc
      r.renderScreen rkc, bind

  addRoute "rkcFront", (bind) ->
      rkcFront.screen =
        name : "rkcFront"
        template: "rkcFront-screen-template"
        views:
          "rkcFront-form": rkcFront
      r.renderScreen rkcFront, bind

  addRoute "rkcOps", (bind) ->
      rkcOps.screen =
        name : "rkcOps"
        template: "rkcOps-screen-template"
        views:
          "rkcOps-form": rkcOps
      r.renderScreen rkcOps, bind

  addRoute "supervisor", (bind) ->
      supervisor.screen =
        name : "supervisor"
        template: "supervisor-screen-template"
        views:
          "action-form": supervisor
      r.renderScreen supervisor, bind

  addRoute "vin", (bind) ->
      vin.screen =
        name : "vin"
        views:
          "vin-form": vin
      r.renderScreen vin, bind

  addRoute "contract/:sub/:id", (bind) ->
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
      timeline.screen =
        name : "timeline"
        template: "timeline-screen-template"
        views:
          "timeline-view": timeline
      r.renderScreen timeline, bind

  Finch.route "rest", (bind) ->
      rest.screen =
        name: "rest"
        views:
          'rest-view': rest
      r.renderScreen rest, bind

  Finch.route "serviceBreak", (bind) ->
      serviceBreak.screen =
        name: "serviceBreak"
        views:
          'break-view': serviceBreak
      r.renderScreen serviceBreak, bind

  Finch.route "na", (bind) ->
      na.screen =
        name: "na"
        views:
          'na-view': na
      r.renderScreen na, bind

  addRoute "search", =>
  Search.attachTo("search")

  addRoute "kpi", =>
  KPI.attachTo("kpi")

  Finch
