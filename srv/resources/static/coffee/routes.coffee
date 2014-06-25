define ["render/screen", "finch", "search/routes"], (r, Finch, Search) ->

  # wrapper which will abort execution when user on a brake
  addRoute = (url, fn) ->
    Finch.route url,
      setup: (bind) ->
        if _.contains(['rest', 'serviceBreak'], Finch.navigate())
          return Finch.abort()
        fn(bind)

  addRoute "back", (bind) ->
    require ["screens/backoffice"], (bo) ->
      bo.screen =
        name : "back"
        template: "back-screen-template"
        views:
          "back-form": bo
      r.renderScreen bo, bind

  addRoute "call/:id", (bind) ->
    require ["screens/call"], (call) ->
      call.screen =
        name : "call"
        template: "call-screen-template"
        views:
          "call-form": call
      r.renderScreen call, bind

  addRoute "case/:id", (bind) ->
    require ["screens/case"], (kase) ->
      kase.screen =
        name : "case"
        template: "case-screen-template"
        views:
          "case-form": kase
      r.renderScreen kase, bind

  addRoute "newCase/:id", (bind) ->
    require ["screens/newCase"], (newCase) ->
      newCase.screen =
        name : "newCase"
        template: "case-screen-template"
        views:
          "case-form": newCase
      r.renderScreen newCase, bind

  addRoute "dict/:dict/:id", (bind) ->
    require ["screens/dictionaries"], (dictionaries) ->
      dictionaries.screen =
        name : "dictionaries"
        template: "dictionaries-screen-template"
        views:
          "dictionaries-view": dictionaries
      r.renderScreen dictionaries, bind

  addRoute "partner/:id", (bind) ->
    require ["screens/partners"], (partner) ->
      partner.screen =
        name : "partner"
        template: "partner-screen-template"
        views:
          "partner-view": partner
      r.renderScreen partner, bind

  addRoute "usermeta/:id", (bind) ->
    require ["screens/user"], (user) ->
      user.screen =
        name : "user"
        views:
          "user-view": user
      r.renderScreen user, bind

  addRoute "uploads", (bind) ->
    require ["screens/uploads"], (uploads) ->
      uploads.screen =
        name : "uploads"
        views:
          "uploads-view": uploads
      r.renderScreen uploads, bind

  addRoute "printSrv/:model/:id", (bind) ->
    require ["screens/printService"], (print) ->
      print.screen =
        name : "printSrv"
        template: "printSrv-screen-template"
        views:
          "print-table": print
      r.renderScreen print, bind

  addRoute "rkc", (bind) ->
    require ["screens/rkc"], (rkc) ->
      rkc.screen =
        name : "rkc"
        template: "rkc-screen-template"
        views:
          "rkc-form": rkc
      r.renderScreen rkc, bind

  addRoute "rkcFront", (bind) ->
    require ["screens/rkcFront"], (rkcFront) ->
      rkcFront.screen =
        name : "rkcFront"
        template: "rkcFront-screen-template"
        views:
          "rkcFront-form": rkcFront
      r.renderScreen rkcFront, bind

  addRoute "rkcOps", (bind) ->
    require ["screens/rkcOps"], (rkcOps) ->
      rkcOps.screen =
        name : "rkcOps"
        template: "rkcOps-screen-template"
        views:
          "rkcOps-form": rkcOps
      r.renderScreen rkcOps, bind

  addRoute "supervisor", (bind) ->
    require ["screens/supervisor"], (supervisor) ->
      supervisor.screen =
        name : "supervisor"
        template: "supervisor-screen-template"
        views:
          "action-form": supervisor
      r.renderScreen supervisor, bind

  addRoute "supervisorOps", (bind) ->
    require ["screens/supervisorOps"], (supervisorOps) ->
      supervisorOps.screen =
        name : "supervisorOps"
        template: "supervisorOps-screen-template"
        views:
          "supervisorOps-table": supervisorOps
      r.renderScreen supervisorOps, bind

  addRoute "vin", (bind) ->
    require ["screens/vin"], (vin) ->
      vin.screen =
        name : "vin"
        views:
          "vin-form": vin
      r.renderScreen vin, bind

  addRoute "reports", (bind) ->
    require ["screens/report"], (report) ->
      report.screen =
        name : "reports"
        template: "reports-screen-template"
        views:
          reports: report
      r.renderScreen report, bind

  addRoute "contract/:sub/:id", (bind) ->
    require ["screens/contract"], (contract) ->
      contract.screen =
        name : "contract"
        template: "contract-screen-template"
        views:
          "contract-form": contract
      # Do not update screen if we stay in the same subprogram. This
      # prevents screen reloading when table rows are clicked on
      # portal screen.
      unless global.previousHash?.match "contract/#{bind.sub}/?"
        global.previousHash = window.location.hash
        r.renderScreen contract, bind

  addRoute "timeline", (bind) ->
    require ["screens/timeline"], (timeline) ->
      timeline.screen =
        name : "timeline"
        template: "timeline-screen-template"
        views:
          "timeline-view": timeline
      r.renderScreen timeline, bind

  Finch.route "rest", (bind) ->
    require ["screens/rest"], (scr) ->
      scr.screen =
        name: "rest"
        views:
          'rest-view': scr
      r.renderScreen scr, bind

  Finch.route "serviceBreak", (bind) ->
    require ["screens/serviceBreak"], (scr) ->
      scr.screen =
        name: "serviceBreak"
        views:
          'break-view': scr
      r.renderScreen scr, bind

  addRoute "search", =>
  Search.attachTo("search")

  Finch
