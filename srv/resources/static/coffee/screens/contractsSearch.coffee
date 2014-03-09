define [ "search/screen"
       , "json!/cfg/model/Contract"
       , "json!/cfg/model/Contract?view=search"
       , "text!tpl/screens/search.html"
       ], (Screen, Contract, Search, tpl) ->

  template: tpl
  constructor: -> Screen.constructor
    searchModels: [Search]
    resultModels: [Contract]
    apiUrl: "/search/contract"
    resultTable: [ { name: 'vin',        fixed: true }
                 , { name: 'cardNumber', fixed: true }
                 , { name: 'plateNum',   fixed: true }
                 , { name: 'name',       fixed: true }
                 , { name: 'phone',      fixed: true }
                 , { name: 'codeWord',   fixed: true }
                 , { name: 'email',      fixed: true }
                 ]
    searchFields: [ "vin"
                    "cardNumber"
                    "plateNum"
                    "name"
                    "phone"
                    "codeWord"
                    "email"
                  ]
    defaultSort: { fields: [{ model: "Contract", name: "id" }], order: "desc" }
    allowedResultFields:
      Contract: [ "vin"
                  "cardNumber"
                  "plateNum"
                  "name"
                  "phone"
                  "codeWord"
                  "email"
                ]
