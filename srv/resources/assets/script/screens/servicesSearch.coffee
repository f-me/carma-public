define [ "search/screen"
       , "screens/search.jade"
       , "json!/cfg/model/Case?view=search"
       , "json!/cfg/model/Service?view=search"
       , "json!/cfg/model/Towage?view=search"
       , "json!/cfg/model/Contract?view=searchCase"
       , "json!/cfg/model/Case"
       , "json!/cfg/model/Service"
       , "json!/cfg/model/Towage"
       , "json!/cfg/model/Contract"

       ], ( Screen
          , tpl
          , CaseSearch
          , ServiceSearch
          , TowageSearch
          , ContractSearch
          , Case
          , Service
          , Towage
          , Contract) ->

  template: tpl()
  constructor: -> Screen.constructor
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
