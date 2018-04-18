###
You see this component at top of the page (on almost every route).
###

{ko} = require "carma/vendor"
{store} = require "carma/neoComponents/store"
actions = require "carma/neoComponents/store/navbar/actions"
smsFormActions = require "carma/neoComponents/store/smsForm/actions"
{NavbarMenuItem} = require "carma/neoComponents/store/navbar/models"

rnd = String(Math.random()).slice 2 # for scope isolation
itemTemplateId = "navbar-item-template--#{rnd}"

# Defining knockout template.
# This done this way to avoid additional wrappers.
do (tpl = require "./itemTemplate.pug") ->
  el = document.createElement "script"
  el.type = "text/html"
  el.id = itemTemplateId
  el.innerHTML = tpl
  document.body.appendChild el


storeSelector = -> store.getState().get "navbar"


class NavbarViewModel
  itemTemplateId: itemTemplateId

  constructor: ->
    # Connector to the store
    @appState = ko.observable storeSelector()
    @unsubscribeFromAppState = store.subscribe => @appState storeSelector()

    @current = ko.pureComputed => @appState().get "current"
    @hidden = ko.pureComputed => @appState().get "hidden"

    @menu = ko.pureComputed =>
      menu = @appState().get "menu"
      hidden = @hidden()
      return menu if hidden is 0
      shownPart = menu.size - hidden

      menu
        .setSize shownPart
        .push new NavbarMenuItem
          name    : "more"
          type    : "dropdown"
          label   : "Ещё"
          screens : menu.slice shownPart

  dispose: =>
    do @unsubscribeFromAppState

  _isAnyChildActive: (list) =>
    return false unless list?
    list.some (x) => @isActive x

  isActive: (model) =>
    current = @current()
    aliases = model.get "nameAliases"
    (aliases? and aliases.includes current) or
      current is model.get("name") or
      @_isAnyChildActive model.get("screens")

  sendBugReport: =>
    store.dispatch actions.sendBugReport()

  showSmsForm: =>
    action = smsFormActions.showSmsForm
    kase = window.global.viewsWare["case-form"]?.knockVM

    caseId = do ->
      x = kase?.id()
      if typeof x is "string" and x isnt "" then Number x else null

    store.dispatch action action.Payload {
      phone:       kase?.contact_phone1()      ? ""
      caseId
      caseCity:    kase?.cityLocal()           ? ""
      caseAddress: kase?.caseAddress_address() ? ""
    }


module.exports =
  componentName: "navbar"

  component:
    template:  require "./template.pug"
    viewModel: NavbarViewModel
