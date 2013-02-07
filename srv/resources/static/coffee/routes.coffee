define [
  "screens/backoffice"
  "screens/call"
  "screens/case"
  "screens/partners"
  "screens/printService"
  "screens/rkc"
  "screens/rkcFront"
  "screens/rkcOps"
  "screens/supervisors"
  "screens/vin"
  "screens/report"
  "screens/editVin"
  "screens/editSms"
  "render/screen"
  ], ( bo, call, kase, partner, print, rkc, rkcFront, rkcOps
     , supervisor, vin, report, editVin, editSms, r) ->
    localScreens: ->
      "case":
        "template": "case-screen-template"
        "views":
          "case-form": kase
      "search":
        "template": "search-screen-template"
        "views":
          "tableView":
             constructor: setupSearchTable
      "back":
        "template": "back-screen-template"
        "views":
          "back-form": bo
      "vin":
        "template": "vin-screen-template"
        "views":
          "vin-form": vin
      "call":
        "template": "call-screen-template"
        "views":
          "call-form": call
      "partner":
        "template": "partner-screen-template"
        "views":
          "partner-view": partner
      "supervisor":
        "template": "supervisor-screen-template"
        "views":
          "action-form": supervisor
      "rkc":
        "template": "rkc-screen-template"
        "views":
          "rkc-form": rkc
      "rkcOps":
        "template": "rkcOps-screen-template"
        "views":
          "rkcOps-form": rkcOps
      "rkcFront":
        "template": "rkcFront-screen-template"
        "views":
          "rkcFront-form": rkcFront
      "reports":
        "template": "reports-screen-template"
        "views":
          "reports": report
      "newVin":
        "template": "newVin-screen-template"
      "editVin":
        "template": "editVin-screen-template"
        "views":
          "vin-form": editVin
      "editSms":
        "template": "editSms-screen-template"
        "views":
          "smsTpl-form": editSms
      "printSrv":
        "template": "printSrv-screen-template"
        "views":
          "print-table": print

    # Setup routing
    localRouter: Backbone.Router.extend
      # Must _not_ end with trailing slashes
      routes:
        "case/:id"    : "loadCase"
        "case"        : "newCase"
        "search"      : "search"
        "vin"         : "vin"
        "back"        : "back"
        "call/:id"    : "loadCall"
        "call"        : "call"
        "reports"     : "reports"
        "partner"     : "newPartner"
        "partner/:id" : "loadPartner"
        "editVin/:id" : "editVin"
        "newVin"      : "newVin"
        "supervisor"  : "supervisor"
        "rkc"         : "rkc"
        "rkcOps"      : "rkcOps"
        "rkcFront"    : "rkcFront"
        "editSms"     : "editSms"
        "printSrv/:model/:id" : "printSrv"

      loadCase    : (id) -> r.renderScreen("case", kase, {"id": id})
      newCase     :      -> r.renderScreen("case", kase, {"id": null})
      search      :      -> renderScreen("search")
      back        :      -> r.renderScreen("back", bo)
      vin         :      -> r.renderScreen("vin", vin)
      newPartner  :      -> r.renderScreen("partner", partner, {"id": null})
      loadPartner : (id) -> r.renderScreen("partner", partner, {"id": id})
      loadCall    : (id) -> r.renderScreen("call", call, {"id": id})
      call        :      -> r.renderScreen("call", call, {"id": null})
      reports     :      -> r.renderScreen("reports", report)
      editVin     : (id) -> r.renderScreen("editVin", editVin, {"id": id})
      newVin      :      -> r.renderScreen("newVin", newVin, {"id": null})
      supervisor  :      -> r.renderScreen("supervisor", supervisor)
      rkc         :      -> r.renderScreen("rkc", rkc)
      rkcOps      :      -> r.renderScreen("rkcOps", rkcOps)
      rkcFront    :      -> r.renderScreen("rkcFront", rkcFront)
      editSms     :      -> r.renderScreen("editSms", editSms)
      printSrv    : (model, id) ->
        renderScreen "printSrv", print, {model: model, id: id}
