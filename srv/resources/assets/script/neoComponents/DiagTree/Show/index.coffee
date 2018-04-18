###
See route #diag/show/%CASE_ID%

To see this component you supposed to:
  1. Edit some "subprogram" dictionary (see #dict route);
  2. Choose some tree root in "diag tree type"
     (in russian "тип дерева неисправностей") field;
  3. Create some "case" (see #case/%CASE_ID%)
  4. Set a program and that subprogram with "diag tree type" set
  5. Now you supposed to see a link "start diagnostics"
     (in russian "начать диагностику"), click it and new window/tab with this
     component will appear
###

{ko} = require "carma/vendor"
{store} = require "carma/neoComponents/store"

{getCaseHistoryRequest} =
  require "carma/neoComponents/store/diagTree/show/actions"

require "./styles.less"

storeSelector = -> store.getState().getIn ["diagTree", "show"]


# It's a wrapper for diag-tree-show-inside, at this point data can be not
# initiated yet, so *-inside component wouldn't be rendered yet, so you may not
# care about data being ready, in *-inside component is always be ready.
# By data being ready I mean existing record in "cases" list, so it means at
# least history is requested already to be fetched.
class DiagTreeShowViewModel
  constructor: ({caseId: caseIdStrParam}) ->
    # Connector to store
    @appState = ko.observable storeSelector()
    @unsubscribeFromAppState = store.subscribe => @appState storeSelector()

    @caseId = ko.pureComputed -> parseInt caseIdStrParam()
    @caseModel = ko.pureComputed => @appState().getIn ["cases", @caseId()], null
    @isInitiated = ko.pureComputed => @caseModel() isnt null
    @isLoading = ko.pureComputed => not @isInitiated()

    unless @isInitiated()
      store.dispatch getCaseHistoryRequest \
        new getCaseHistoryRequest.Payload caseId: @caseId()

  dispose: =>
    do @unsubscribeFromAppState


module.exports =
  componentName: "diag-tree-show"

  component:
    template:  require "./template.pug"
    viewModel: DiagTreeShowViewModel
