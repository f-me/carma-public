define [ "search/screen"
       , "screens/search.jade"
       , "q"
       ], ( Screen
          , tpl
          Q) ->

  fetchJson = (done) ->
    Q.all(
      [fetch('/cfg/model/Case?view=search', {credentials: 'same-origin'})
      ,fetch('/cfg/model/Service?view=search', {credentials: 'same-origin'})
      ,fetch('/cfg/model/Towage?view=search', {credentials: 'same-origin'})
      ,fetch('/cfg/model/Contract?view=searchCase', {credentials: 'same-origin'})
      ,fetch('/cfg/model/Case', {credentials: 'same-origin'})
      ,fetch('/cfg/model/Service', {credentials: 'same-origin'})
      ,fetch('/cfg/model/Towage', {credentials: 'same-origin'})
      ,fetch('/cfg/model/Contract', {credentials: 'same-origin'})
      ]).done((res) -> done(res.map((x) -> x.json())))

  template: tpl()
  constructor: -> fetchJson (
          [ CaseSearch
          , ServiceSearch
          , TowageSearch
          , ContractSearch
          , Case
          , Service
          , Towage
          , Contract]) -> Screen.constructor
    apiUrl: "/search/case"
    searchModels: [CaseSearch, ServiceSearch, TowageSearch, ContractSearch]
    resultModels: [Case, Service, Towage, Contract]
    resultTable: [ { name: 'Case_id', fixed: true }
                 , { name: 'contact'              }
                 , { name: 'callDate'             }
                 , { name: 'phone'                }
                 , { name: "customerComment"      }
                 , { name: 'vin'                  }
                 , { name: 'program'              }
                 ]
    searchFields: [ "callDate"
                    "createTime"
                    "Case_id"
                    "phone"
                    "contact"
                    "vin"
                    "plateNum"
                  ]
    defaultSort: { fields: [ { model: "Case", name: "id" } ], order: "desc" }
    allowedResultFields:
      Case: [
        "id"
        "contact_phone1"
        "contact_phone2"
        "contact_phone3"
        "contact_phone4"
        "contact_ownerPhone1"
        "contact_ownerPhone2"
        "contact_ownerPhone3"
        "contact_ownerPhone4"
        "contact_name"
        "contact_ownerName"
        "contact_contactOwner"
        "callDate"
        "car_plateNum"
        "car_vin"
        "program"
        "city"
        "car_make"
        "car_model"
        "comment"
        "callTaker"
        "customerComment"
        ]
      Service: [
        "type"
        "contractor_partnerId"
        "createTime"
        ]
      Towage: [
        "towDealer_partnerId"
        ]
      Contract: [
        "cardNumber"
        ]
