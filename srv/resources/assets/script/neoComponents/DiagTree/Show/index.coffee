{ko} = require "carma/vendor"


class DiagTreeShowViewModel
  constructor: ({@caseId}) ->
  dispose: =>


module.exports =
  componentName: "diag-tree-show"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeShowViewModel
