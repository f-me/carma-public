{Immutable: {Record}} = require "carma/vendor"
{makeActions} = require "carma/neoComponents/store/utils"
{bugReport} = require "carma/lib/bug-report"

actions = makeActions __dirname,

  fillMenu:
    Payload: Record
      plainData: []

  setCurrent:
    Payload: Record
      name: ""

  sendBugReport:
    handler: -> do bugReport.sendReport

  hide:
    Payload: Record
      count: 1 # How many elements to hide to dropdown

  resetHidden: null

module.exports = actions
