define [ "search/screen"
       , "screens/search.jade"
       , "json!/cfg/model/Call"
       , "json!/cfg/model/Call?view=search"
       ], ( Screen
          , tpl
          , callModel
          , callSearchModel) ->

  template: tpl()
  constructor: -> Screen.constructor
    searchModels: [callSearchModel]
    resultModels: [callModel]
    apiUrl: "/search/call"
    resultTable: [ { name: 'phone',    fixed: true }
                 , { name: 'callDate', fixed: true }
                 , { name: 'program'               }
                 , { name: 'callTaker'             }
                 ]
    searchFields: [ "phone",
                    "callDate"
                    "callTaker"
                    "callType"
                    "program"
                    "caller"
                  ]
    defaultSort: { fields: [{ model: "Call", name: "id" }], order: "desc" }
    allowedResultFields:
      Call: [ "callDate"
              "callerPhone"
              "callerName"
              "program"
              "callTaker"
              "callType"
            ]
