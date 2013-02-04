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
  ], ( bo, call, kase, partner, print, rkc, rkcFront, rkcOps
     , supervisor, vin, report, editVin, editSms) ->
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

      loadCase    : (id) -> renderScreen("case", {"id": id})
      newCase     :      -> renderScreen("case", {"id": null})
      search      :      -> renderScreen("search")
      back        :      -> renderScreen("back")
      vin         :      -> renderScreen("vin")
      newPartner  :      -> renderScreen("partner", {"id": null})
      loadPartner : (id) -> renderScreen("partner", {"id": id})
      loadCall    : (id) -> renderScreen("call", {"id": id})
      call        :      -> renderScreen("call", {"id": null})
      reports     :      -> renderScreen("reports")
      editVin     : (id) -> renderScreen("editVin", {"id": id})
      newVin      :      -> renderScreen("newVin", {"id": null})
      supervisor  :      -> renderScreen("supervisor")
      rkc         :      -> renderScreen("rkc")
      rkcOps      :      -> renderScreen("rkcOps")
      rkcFront    :      -> renderScreen("rkcFront")
      editSms     :      -> renderScreen("editSms")
      printSrv    : (model, id) -> renderScreen "printSrv", {model: model, id: id}
