{ko} = require "carma/vendor"
{store} = require "carma/neoComponents/store"

{} =
  require "carma/neoComponents/store/diagTree/edit/actions"

require "./styles.less"

storeSelector = -> store.getState().getIn ["diagTree", "edit"]


class DiagTreeEditorInsideViewModel
  constructor: ->
    # Connector to store
    @appState = ko.observable storeSelector()
    @unsubscribeFromAppState = store.subscribe => @appState storeSelector()

  dispose: =>
    do @unsubscribeFromAppState

  addSlide: =>
    console.error "TODO 'addSlide' method", @appState().get("slides").toJS()


module.exports =
  componentName: "diag-tree-editor-inside"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeEditorInsideViewModel
