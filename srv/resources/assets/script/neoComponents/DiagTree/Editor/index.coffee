{ko} = require "carma/vendor"
require "./styles.less"


class DiagTreeEditorViewModel
  constructor: ->
  dispose: =>


module.exports =
  componentName: "diag-tree-editor-wrap"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeEditorViewModel
