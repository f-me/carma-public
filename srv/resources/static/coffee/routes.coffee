define ["render/screen", "finch", "search/routes"], (r, Finch, Search) ->

  Finch.route "back", (bind) ->
    require ["screens/backoffice"], (bo) ->
      bo.screen =
        name : "back"
        template: "back-screen-template"
        views:
          "back-form": bo
      r.renderScreen bo, bind

  Finch.route "call/:id", (bind) ->
    require ["screens/call"], (call) ->
      call.screen =
        name : "call"
        template: "call-screen-template"
        views:
          "call-form": call
      r.renderScreen call, bind

  Finch.route "case/:id", (bind) ->
    require ["screens/case"], (kase) ->
      kase.screen =
        name : "case"
        template: "case-screen-template"
        views:
          "case-form": kase
      r.renderScreen kase, bind

  Finch.route "newCase/:id", (bind) ->
    require ["screens/newCase"], (newCase) ->
      newCase.screen =
        name : "newCase"
        template: "case-screen-template"
        views:
          "case-form": newCase
      r.renderScreen newCase, bind

  Finch.route "dict/:dict/:id", (bind) ->
    require ["screens/dictionaries"], (dictionaries) ->
      dictionaries.screen =
        name : "dictionaries"
        template: "dictionaries-screen-template"
        views:
          "dictionaries-view": dictionaries
      r.renderScreen dictionaries, bind

  Finch.route "partner/:id", (bind) ->
    require ["screens/partners"], (partner) ->
      partner.screen =
        name : "partner"
        template: "partner-screen-template"
        views:
          "partner-view": partner
      r.renderScreen partner, bind

  Finch.route "usermeta/:id", (bind) ->
    require ["screens/user"], (user) ->
      user.screen =
        name : "user"
        views:
          "user-view": user
      r.renderScreen user, bind

  Finch.route "uploads", (bind) ->
    require ["screens/uploads"], (uploads) ->
      uploads.screen =
        name : "uploads"
        views:
          "uploads-view": uploads
      r.renderScreen uploads, bind

  Finch.route "printSrv/:model/:id", (bind) ->
    require ["screens/printService"], (print) ->
      print.screen =
        name : "printSrv"
        template: "printSrv-screen-template"
        views:
          "print-table": print
      r.renderScreen print, bind

  Finch.route "rkc", (bind) ->
    require ["screens/rkc"], (rkc) ->
      rkc.screen =
        name : "rkc"
        template: "rkc-screen-template"
        views:
          "rkc-form": rkc
      r.renderScreen rkc, bind

  Finch.route "rkcFront", (bind) ->
    require ["screens/rkcFront"], (rkcFront) ->
      rkcFront.screen =
        name : "rkcFront"
        template: "rkcFront-screen-template"
        views:
          "rkcFront-form": rkcFront
      r.renderScreen rkcFront, bind

  Finch.route "rkcOps", (bind) ->
    require ["screens/rkcOps"], (rkcOps) ->
      rkcOps.screen =
        name : "rkcOps"
        template: "rkcOps-screen-template"
        views:
          "rkcOps-form": rkcOps
      r.renderScreen rkcOps, bind

  Finch.route "supervisor", (bind) ->
    require ["screens/supervisor"], (supervisor) ->
      supervisor.screen =
        name : "supervisor"
        template: "supervisor-screen-template"
        views:
          "action-form": supervisor
      r.renderScreen supervisor, bind

  Finch.route "supervisorOps", (bind) ->
    require ["screens/supervisorOps"], (supervisorOps) ->
      supervisorOps.screen =
        name : "supervisorOps"
        template: "supervisorOps-screen-template"
        views:
          "supervisorOps-table": supervisorOps
      r.renderScreen supervisorOps, bind

  Finch.route "vin", (bind) ->
    require ["screens/vin"], (vin) ->
      vin.screen =
        name : "vin"
        views:
          "vin-form": vin
      r.renderScreen vin, bind

  Finch.route "reports", (bind) ->
    require ["screens/report"], (report) ->
      report.screen =
        name : "reports"
        template: "reports-screen-template"
        views:
          reports: report
      r.renderScreen report, bind

  Finch.route "contract/:sub/:id", (bind) ->
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

  Finch.route "timeline", (bind) ->
    require ["screens/timeline"], (timeline) ->
      timeline.screen =
        name : "timeline"
        template: "timeline-screen-template"
        views:
          "timeline-form": timeline
      r.renderScreen timeline, bind

  Search.attachTo("search")

  Finch
