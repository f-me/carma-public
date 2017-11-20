{ko} = require "carma/vendor"


class DiagTreeEditorViewModel
  constructor: ->
    console.error "constructor of DiagTreeEditorViewModel"
  dispose: =>


module.exports =
  componentName: "diag-tree-editor"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeEditorViewModel
