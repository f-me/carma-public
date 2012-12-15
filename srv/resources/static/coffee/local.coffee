#/ Everything local to the customer resides here

localScreens = ->
  "case":
    "template": "case-screen-template"
    "views":
      "case-form":
        constructor: setupCaseMain
        destructor: removeCaseMain
  "search":
    "template": "search-screen-template"
    "views":
      "tableView":
         constructor: setupSearchTable
  "back":
    "template": "back-screen-template"
    "views":
      "back-form":
        constructor: setupBackOffice
        destructor:  removeBackOffice
  "vin":
    "template": "vin-screen-template"
    "views":
      "vin-form":
        constructor: setupVinForm
  "call":
    "template": "call-screen-template"
    "views":
      "call-form":
        constructor: setupCallForm
  "partner":
    "template": "partner-screen-template"
    "views":
      "partner-view":
        constructor: setupPartnersForm
        destructor: releasePartnersForm
  "supervisor":
    "template": "supervisor-screen-template"
    "views":
      "action-form":
        constructor: setupSupervisorScreen
  "rkc":
    "template": "rkc-screen-template"
    "views":
      "rkc-form":
        constructor: setupRKCScreen
        destructor: removeRKCScreen
  "rkcOps":
    "template": "rkcOps-screen-template"
    # "views":
    #   "rkcOps-form":
    #     constructor: setupRKCOpsScreen
    #     destructor: removeRKCOpsScreen
  "reports":
    "template": "reports-screen-template"
    "views":
      "reports":
        constructor: setupReports
  "newVin":
    "template": "newVin-screen-template"
  "editVin":
    "template": "editVin-screen-template"
    "views":
      "vin-form":
        constructor: setupEditVin
  "editSms":
    "template": "editSms-screen-template"
    "views":
      "smsTpl-form":
        constructor: setupSmsTplForm

# Setup routing
localRouter = Backbone.Router.extend
  # Must _not_ end with trailing slashes
  routes:
    "case/:id"    : "loadCase"
    "case"        : "newCase"
    "search"      : "search"
    "vin"         : "vin"
    "back"        : "back"
    "call/:id"    : "loadCall"
    "call"        : "call"
    "reports"     : "reports"
    "partner"     : "newPartner"
    "partner/:id" : "loadPartner"
    "editVin/:id" : "editVin"
    "newVin"      : "newVin"
    "supervisor"  : "supervisor"
    "rkc"         : "rkc"
    "rkcOps"      : "rkcOps"
    "editSms"     : "editSms"

  loadCase    : (id) -> renderScreen("case", {"id": id})
  newCase     :      -> renderScreen("case", {"id": null})
  search      :      -> renderScreen("search")
  back        :      -> renderScreen("back")
  vin         :      -> renderScreen("vin")
  newPartner  :      -> renderScreen("partner", {"id": null})
  loadPartner : (id) -> renderScreen("partner", {"id": id})
  loadCall    : (id) -> renderScreen("call", {"id": id})
  call        :      -> renderScreen("call")
  reports     :      -> renderScreen("reports")
  editVin     : (id) -> renderScreen("editVin", {"id": id})
  newVin      :      -> renderScreen("newVin")
  supervisor  :      -> renderScreen("supervisor")
  rkc         :      -> renderScreen("rkc")
  rkcOps        :     -> renderScreen("rkcOps")
  editSms     :      -> renderScreen("editSms")

# here is entry point
$ ->
  $.getJSON "/cfg/dictionaries",          (dicts)  ->
    $.getJSON "/_whoami/",                  (user)   ->
      $.getJSON "/cfg/models",                (models) ->
        $.getJSON "/s/screens",                 (nav)    ->
          $.getJSON "/usersDict",                 (users)  ->
            dicts.users =
                entries:
                    for i in users
                           {value: i.value, label: "#{i.label} (#{i.value})"}
              dicts.roles =
                entries:
                    for i in users
                           {value: i.value, label: i.roles }
              mainSetup localScreens(),
                        localRouter,
                        dicts,
                        hooks(),
                        user,
                        models
              global.nav = filterScreenPerms nav
              global.keys = {}
              global.keys.arrows = {left: 37, up: 38, right: 39, down: 40 }
              ko.applyBindings global.nav, $('#nav')[0]
              ext = user.meta.avayaExt
              pwd = user.meta.avayaPwd
              if ext and pwd
                global.avayaPhone = new AvayaWidget($('#avaya-panel'), ext, pwd)
              if window.location.hash == ""
                redirectToHomePage user

window.onerror = (msg, url, line) ->
  $.ajax
    type: "POST"
    url : "/errors"
    data: "#{msg} #{url} #{line}"
  return false

this.redirectToHomePage = (user) ->
  mainRole = user.roles[0]
  if mainRole == "front"
    homePage = "call"
  else if mainRole == "back"
    homePage = "back"
  global.router.navigate(homePage, {trigger: true})

filterScreenPerms = (nav) ->
  nav.screens = fScrnPerms(nav)
  return nav

fScrnPerms = (nav) ->
  p = global.user.roles
  nav.screens =
    for s in nav.screens when not _.isEmpty _.intersection(s.permissions, p)
      s.screens = fScrnPerms(s) if s.screens
      s
  return nav.screens

# Model method HTTP access point wrt redson location
this.modelMethod = (modelName, method) -> "/_/#{modelName}/#{method}"

this.mkServicesDescs = (p, s) ->
  description: getServiceDesc(p ,s.modelName())
  title:       s.modelTitle

this.getServiceDesc = (program, service) ->
  si  = global.dictionaries['ServiceInfo'][program]?[service]
  si ?= global.dictionaries['ServiceInfo']['default']?[service]

# jquery -> html(as string) conversion, with selected element
jQuery.fn.outerHTML = () -> jQuery("<div>").append(this.clone()).html()

# like _.has but for list
this.hasL = (lst, e) -> _.find(lst, (x) -> x == e)

# Standard element callback which will scroll model into view and
# focus on first field
this.stdElCb = (elName) ->
  e = $el(elName)
  # Scroll group to the top of the screen
  if e.hasClass("accordion-inner")
    e.parents(".accordion-group")[0].scrollIntoView()
  f = e.find(".focusable")[0]
  f and f.focus()

# Scroll case field into view and focus
this.focusField = (name) ->
  e = $("#main-container").find("[name=" + name + "]")[0]
  e.scrollIntoView()
  e.focus()

# Find VM of reference in a case by its view name. 
this.findCaseOrReferenceVM = (view) ->
  kase = global.viewsWare["case-form"].knockVM
  if (view is "case-form")
    kase
  else
    _.find kase.servicesReference(), (svc) -> svc.view is view

# Find VM of a view, properly handling reference views or views of
# field groups. If the view name is "case-form", then return knockVM
# for case.
this.findVM = (view) ->
  vw = global.viewsWare[view]
  if vw and vw.parentView?
    # Find VM of a group rendered in a view.
    findCaseOrReferenceVM(vw.parentView)
  else
    findCaseOrReferenceVM(view)


# Strip whitespace from string
this.stripWs = (s) -> do (s) -> s.replace(/\s+/g, '')


# Given a string of form "foo/bar", return object with fields
# `view=foo` and `field=bar`. If input is of form "bar", then `view`
# field is equal to defaultView.
#
# This is used to parse field references in meta annotations such as
# targetCoords or targetAddr.
this.splitFieldInView = (input, defaultView) ->
  chunks = input.split('/')
  if chunks.length > 1
    view_name = chunks[0]
    field_name = chunks[1]
  else
    view_name = defaultView
    field_name = chunks[0]
    
  obj =
    view: view_name
    field: field_name

# Hide all views on center pane and show view for first reference
# stored in <fieldName> of model loaded into <parentView> there
this.showComplex = (parentView, fieldName) ->
  depViewName = global.viewsWare[parentView].depViews[fieldName][0]
  view = $el(depViewName)

  return if view.is(':visible')
  $(".complex-field").hide()

  view.show ->
    isDealerView = depViewName.match(/towDealer_partner-view/)
    isPartnerView = depViewName.match(/contractor_partner-view/)
    if isDealerView or isPartnerView
      initPartnerTables view, parentView

    initOSM(e, parentView) for e in view.find(".osMap")

this.hideComplex = ->
  $(".complex-field").hide()
  $(".default-complex-field").show()
       
# Dispatch on some picker type
#
# In templates, bind click to 'doPick({{meta.picker}}, ...,
# event.target)' to call the appropriate picker.
this.doPick = (pickType, args, elt) ->
  pickers =

    callPlease: (modelName) ->
      bb = global.viewsWare["call-form"].bbInstance
      number = bb.get(modelName)
      global.avayaPhone && global.avayaPhone.call(number)

    geoPicker: geoPicker
    reverseGeoPicker: reverseGeoPicker
    mapPicker: mapPicker
  pickers[pickType](args, elt)

this.kdoPick = (pickType, args, k, e) ->
  doPick pickType, args, e.srcElement if e.ctrlKey and e.keyCode == k

this.mkDataTable = (t, opts) ->
  defaults =
    sScrollY  : "500px"
    bPaginate : false
    oLanguage :
      sSearch      : "Фильтр"
      sInfoEmpty   : ""
      sZeroRecords : "Ничего не найдено"
      sInfo        : "Показаны записи с _START_ по _END_ (всего _TOTAL_)"

  defaults = $.extend(defaults, opts) if opts?

  t.dataTable defaults

# FIXME: This could be a callback for main.js:saveInstance
this.successfulSave = ->
  $span = $(this).siblings(".save-result")
  setTimeout((->
    $span.text("Сохранено успешно")
    $span.show()
    $span.fadeOut(2000))
  , 500)

this.checkAccordion = (e) ->
  acc = e.parents('.accordion-body') #.hasClass('in')
  return if acc.hasClass('in')
  acc.collapse('show')

this.getWeather = (city, cb) ->
  url = "/#{city}"
  $.getJSON "/weather/#{city}", (data) -> cb(data)

this.focusRef = (kvm) ->
  e = $('#' + kvm['view'])
  e.parent().prev()[0].scrollIntoView()
  e.find('input')[0].focus()
  e.find('input').parents(".accordion-body").first().collapse('show')

this.bindRemove = (parent, field, cb) ->
  for i in parent["#{field}Reference"]()
    do (i) ->
      $("##{i['view']}")
        .parents('div.accordion-group')
        .first()
        .find('.icon.icon-remove')
        .click ->
          removeReference(parent, field, i)
          bindRemove parent, field, cb
          cb(parent, field, i) if _.isFunction cb

this.bindDelete = (parent, field, cb) ->
  bindRemove parent, field, (p, f, kvm) ->
    deleteCb = (args...) -> cb(args) if _.isFunction cb
    $.ajax
      'type'     : 'DELETE'
      'url'      : "/_/#{kvm.modelName()}/#{kvm.id()}"
      'success'  : -> deleteCb
      'error'    : (xhr) ->
        if xhr.status == 404
          deleteCb(d.acc())
        else
          alert 'error'

################################################################################
# utils
this.toUnix = (d) -> Math.round(d.getTime() / 1000)

# flip . setTimeout
this.sTout = (wait, fn) -> setTimeout fn, wait
