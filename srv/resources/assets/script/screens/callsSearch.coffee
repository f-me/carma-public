define [ "search/screen"
       , "screens/search.jade"
       , "q"
       ],
       (Screen, tpl, Q) ->

  fetchJson = (done) ->
    Q.all(
      [fetch('/cfg/model/Call', {credentials: 'same-origin'})
      ,fetch('/cfg/model/Call?view=search', {credentials: 'same-origin'})
      ]).done((res) -> done(res.map((x) -> x.json())))

  template: tpl()
  constructor: -> fetchJson ([Call, Search]) -> Screen.constructor
    searchModels: [Search]
    resultModels: [Call]
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
