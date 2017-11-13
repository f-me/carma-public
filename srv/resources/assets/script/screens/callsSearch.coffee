{tpl} = require "carma/lib/template"

Screen = require "carma/search/screen"
{data} = require "carma/data"

template = tpl require "carma-tpl/screens/search.pug"

callModel       = data.cfg.m.Call
callSearchModel = data.cfg.m.v.search.Call

module.exports = {
  template
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
}
