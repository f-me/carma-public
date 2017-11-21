{Immutable: {Record}} = require "carma/vendor"
{makeActions, catchFailure} = require "carma/neoComponents/store/utils"

actions = makeActions __dirname,
  fooAction: null

module.exports = actions
