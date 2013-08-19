define [
  "screens/backoffice"
  "screens/call"
  "screens/case"
  "screens/newCase"
  "screens/dictionaries"
  "screens/partners"
  "screens/user"
  "screens/uploads"
  "screens/printService"
  "screens/rkc"
  "screens/rkcFront"
  "screens/rkcOps"
  "screens/supervisor"
  "screens/supervisorOps"
  "screens/vin"
  "screens/report"
  "screens/contract"
  "screens/editVin"
  "screens/newVin"
  "screens/editSms"
  "screens/program"
  "screens/partnersSearch"
  "render/screen"
  ], ( bo
     , call
     , kase
     , newCase
     , dictionaries
     , partner
     , user
     , uploads
     , print
     , rkc
     , rkcFront
     , rkcOps
     , supervisor
     , supervisorOps
     , vin
     , report
     , contract
     , editVin
     , newVin
     , editSms
     , program
     , partnersSearch
     , r) ->
    localScreens: ->
      "case":
        "template": "case-screen-template"
        "views":
          "case-form": kase
      "newCase":
        "template": "case-screen-template"
        "views":
          "case-form": newCase
      "dictionaries":
        "template": "dictionaries-screen-template"
        "views":
          "dictionaries-view": dictionaries
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
      "user":
        "template": "user-screen-template"
        "views":
          "user-view": user
      "uploads":
        "template": "uploads-screen-template"
        "views":
          "user-view": uploads
      "supervisor":
        "template": "supervisor-screen-template"
        "views":
          "action-form": supervisor
      "supervisorOps":
        "template": "supervisorOps-screen-template"
        "views":
          "supervisorOps-table": supervisorOps
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
      "contract":
        "template": "contract-screen-template"
        "views":
          "contract-form": contract
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
      "program":
        "template": "program-screen-template"
        "views":
          "program-view": program
      "partnersSearch":
        "views":
          "search-view": partnersSearch

    # Setup routing
    localRouter: Backbone.Router.extend
      # Must _not_ end with trailing slashes
      routes:
        "case/:id"       : "loadCase"
        "newCase/:id"    : "loadNewCase"
        "dictionaries/:dict" : "dictionaries"
        "dictionaries/:dict/:id" : "editDictionary"
        "search"         : "search"
        "uploads"        : "uploads"
        "vin"            : "vin"
        "back"           : "back"
        "call/:id"       : "loadCall"
        "call"           : "call"
        "reports"        : "reports"
        "contract/:p"    : "newContract"
        "contract/:p/:id": "getContract"
        "partner"        : "newPartner"
        "partner/:id"    : "loadPartner"
        "usermeta"       : "newUser"
        "usermeta/:id"   : "loadUser"
        "editVin/:id"    : "editVin"
        "newVin"         : "newVin"
        "supervisor"     : "supervisor"
        "supervisorOps"  : "supervisorOps"
        "rkc"            : "rkc"
        "rkcOps"         : "rkcOps"
        "rkcFront"       : "rkcFront"
        "editSms"        : "editSms"
        "program"        : "program"
        "program/:id"    : "loadProgram"
        "printSrv/:model/:id" : "printSrv"
        "partnersSearch"        : "partnersSearch"
        "partnersSearch/:model" : "partnersSearchModel"

      loadCase      : (id) -> r.renderScreen("case", kase, {"id": id})
      loadNewCase   : (id) -> r.renderScreen("newCase", newCase, {"id": id})
      dictionaries  : (dict) -> r.renderScreen("dictionaries", dictionaries, {dict})
      editDictionary : (dict, id) -> r.renderScreen("dictionaries", dictionaries, {dict, id})
      search        :      -> renderScreen("search")
      uploads       :      -> r.renderScreen("uploads", uploads)
      back          :      -> r.renderScreen("back", bo)
      vin           :      -> r.renderScreen("vin", vin)
      newPartner    :      -> r.renderScreen("partner", partner, {"id": null})
      loadPartner   : (id) -> r.renderScreen("partner", partner, {"id": id})
      newUser       :      -> r.renderScreen("user", user, {"id": null})
      loadUser      : (id) -> r.renderScreen("user", user, {"id": id})
      loadCall      : (id) -> r.renderScreen("call", call, {"id": id})
      call          :      -> r.renderScreen("call", call, {"id": null})
      reports       :      -> r.renderScreen("reports", report)
      newContract   :(p)   -> r.renderScreen "contract", contract,
                                {"program": p, "id": null}
      getContract   :(p,id) -> r.renderScreen "contract", contract,
                                {"program": p, "id": id}
      editVin       : (id) -> r.renderScreen("editVin", editVin, {"id": id})
      newVin        :      -> r.renderScreen("newVin", newVin, {"id": null})
      supervisor    :      -> r.renderScreen("supervisor", supervisor)
      supervisorOps :      -> r.renderScreen("supervisorOps", supervisorOps)
      rkc           :      -> r.renderScreen("rkc", rkc)
      rkcOps        :      -> r.renderScreen("rkcOps", rkcOps)
      rkcFront      :      -> r.renderScreen("rkcFront", rkcFront)
      editSms       :      -> r.renderScreen("editSms", editSms)
      program       :      -> r.renderScreen("program", program)
      loadProgram   : (id) -> r.renderScreen("program", program, {"id": id})
      printSrv      : (model, id) ->
        r.renderScreen "printSrv", print, {model: model, id: id}
      partnersSearch     : -> r.renderScreen("partnersSearch", partnersSearch)
      partnersSearchModel: (model) ->
        r.renderScreen "partnersSearch", partnersSearch, {model: model}
