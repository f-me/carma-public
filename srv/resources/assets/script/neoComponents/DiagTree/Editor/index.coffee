###
See route #diag/edit

This is a iframe-wrapper for new "pure" implementation of this editor.
###

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
