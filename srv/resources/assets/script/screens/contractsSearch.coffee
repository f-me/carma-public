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
    # If full VIN and subprogram given, also search in ARC database
    searchHook: (q) ->
      if q.hook
        q.hook = false
      else
        vin = q.searchParams().predicates.vin
        subprogram = q.searchParams().predicates.subprogram
        if vin?.length == 17 && subprogram?
          arcQuery = "/arcImport/#{vin}?subprogram=#{subprogram}"
          $.getJSON arcQuery, (res) ->
            if res[0] > 0
              # Prevent infinite searchHook loop
              q.hook = true
              q.search()
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
      Contract: _.without(_.pluck(Contract.fields, 'name'), 'id', 'dixi')
