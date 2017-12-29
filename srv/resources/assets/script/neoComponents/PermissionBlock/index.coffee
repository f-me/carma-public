{ko} = require "carma/vendor"
{store} = require "carma/neoComponents/store"

{saveModelInstanceRequest} =
  require "carma/neoComponents/store/model/actions"

require "./styles.less"


class PermissionBlockViewModel
  constructor: ({
    @readonly # boolean
    @viewName # string
  }) ->
    @subscriptions = [] # Mutable

    @saveStatus = ko.observable null

    @saveStatusFadeOut = ko.pureComputed @saveStatus
      .extend rateLimit: {method: "notifyWhenChangesStop", timeout: 100}

  dispose: =>
    do x.dispose for x in @subscriptions

  onSaveClick: =>
    @saveStatus null
    action = saveModelInstanceRequest
    store.dispatch action new action.Payload {@viewName}
      .then  => @saveStatus "success"
      .catch => @saveStatus "failure"
      .then  => setTimeout (=> @saveStatus null), 2300 # gap for animation


module.exports =
  componentName: "permission-block"

  component:
    template:  require "./template.pug"
    viewModel: PermissionBlockViewModel
