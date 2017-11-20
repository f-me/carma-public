{ko} = require "carma/vendor"


class DiagTreeEditorViewModel
  constructor: ->
  dispose: =>


module.exports =
  componentName: "diag-tree-editor"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeEditorViewModel
