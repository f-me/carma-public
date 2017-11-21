{ko} = require "carma/vendor"
require "./styles.less"


class DiagTreeEditorViewModel
  constructor: ->
  dispose: =>

  addSlide: =>
    console.error "TODO 'addSlide' method"


module.exports =
  componentName: "diag-tree-editor"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeEditorViewModel
