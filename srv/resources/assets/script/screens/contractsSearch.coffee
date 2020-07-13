{$, _} = require "carma/vendor"

Screen   = require "carma/search/screen"
{data}   = require "carma/data"
template = require "carma-tpl/screens/search.pug"

Contract = data.cfg.m.Contract
Search   = data.cfg.m.v.search.Contract

module.exports = {
  template
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
      Contract: _.without(_.pluck(Contract.fields, 'name'), 'id', 'dixi')
}
