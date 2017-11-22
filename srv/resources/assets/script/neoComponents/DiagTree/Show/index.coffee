{ko} = require "carma/vendor"
{backgrounds} = require "./precompiled"
require "./styles.less"

yesNoRegs = [/^да/i, /^нет/i]


class DiagTreeShowViewModel
  constructor: ({@caseId}) ->
  dispose: =>


module.exports =
  componentName: "diag-tree-show"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeShowViewModel
