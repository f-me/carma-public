{ko} = require "carma/vendor"
{backgrounds} = require "./precompiled"
{store} = require "carma/neoComponents/store"

{
  getCaseHistoryRequest
} = require "carma/neoComponents/store/diagTree/show/actions"

require "./styles.less"

yesNoRegs = [/^да/i, /^нет/i]


class DiagTreeShowViewModel
  constructor: ({@caseId}) ->
    @slideHeader = ko.observable "testing slide header"

    store.dispatch getCaseHistoryRequest \
      getCaseHistoryRequest.Payload caseId: @caseId()

  dispose: =>


module.exports =
  componentName: "diag-tree-show"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeShowViewModel
