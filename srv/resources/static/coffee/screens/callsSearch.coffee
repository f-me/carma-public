define [ "search/screen"
       , "text!tpl/screens/search.html"
       , "json!/cfg/model/Call"
       , "json!/cfg/model/Call?view=search"
       ], ( Screen
          , tpl
          , callModel
          , callSearchModel) ->

  template: tpl
  constructor: -> Screen.constructor
    searchModels: [callSearchModel]
    resultModels: [callModel]
    apiUrl: "/search/call"
    resultTable: [ { name: 'phone',    fixed: true }
                 , { name: 'callDate', fixed: true }
                 , { name: 'program'               }
                 , { name: 'wazzup'                }
                 , { name: 'callTaker'             }
                 ]
    searchFields: [ "phone",
                    "callDate"
                    "callTaker"
                    "callType"
                    "program"
                    "wazzup"
                    "caller"
                  ]
    defaultSort: { fields: [{ model: "Call", name: "id" }], order: "desc" }
    allowedResultFields:
      Call: [ "callDate"
              "callerName_phone1"
              "callerName_phone2"
              "callerName_phone3"
              "callerName_phone4"
              "callerName_ownerPhone1"
              "callerName_ownerPhone2"
              "callerName_ownerPhone3"
              "callerName_ownerPhone4"
              "callerName_name"
              "callerName_ownerName"
              "program"
              "wazzup"
              "callTaker"
              "callType"
            ]
