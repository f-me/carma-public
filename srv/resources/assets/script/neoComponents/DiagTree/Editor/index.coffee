{ko} = require "carma/vendor"
{store} = require "carma/neoComponents/store"

{loadSlidesRequest} =
  require "carma/neoComponents/store/diagTree/edit/actions"

require "./styles.less"

storeSelector = -> store.getState().getIn ["diagTree", "edit"]


class DiagTreeEditorViewModel
  constructor: ->
    # Connector to store
    @appState = ko.observable storeSelector()
    @unsubscribeFromAppState = store.subscribe => @appState storeSelector()

    @isLoading = ko.pureComputed => @appState().get "isSlidesLoading"
    @isLoaded  = ko.pureComputed => @appState().get "isSlidesLoaded"
    @isFailed  = ko.pureComputed => @appState().get "isSlidesLoadingFailed"

    @isParsingDataFailed = ko.pureComputed =>
      @appState().get "isParsingSlidesDataFailed"

    if not @isLoading() and not @isLoaded()
      store.dispatch loadSlidesRequest()

  dispose: =>
    do @unsubscribeFromAppState


module.exports =
  componentName: "diag-tree-editor"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeEditorViewModel
