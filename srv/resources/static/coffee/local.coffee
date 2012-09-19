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
      "partner-form":
        constructor: setupPartnersForm
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

# here is entry point
$ ->
  $.getJSON "/cfg/dictionaries",          (dicts)  ->
    $.getJSON "/_whoami/",                  (user)   ->
      $.getJSON "/s/js/data/conditions.json", (checks) ->
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
              global.checks = checks
              global.keys = {}
              global.keys.arrows = {left: 37, up: 38, right: 39, down: 40 }
              ko.applyBindings global.nav, $('#nav')[0]
              ext = user.meta.avayaExt
              pwd = user.meta.avayaPwd
              if ext and pwd
                global.avayaPhone = new AvayaWidget($('#avaya-panel'), ext, pwd)
              if window.location.hash == ""
                redirectToHomePage user

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
  e = $("#case-form").find("[name=" + name + "]")[0]
  e.scrollIntoView()
  e.focus()


# Hide all views on center pane and show view for first reference
# stored in <fieldName> of model loaded into <parentView> there
this.showComplex = (parentView, fieldName) ->
  depViewName = global.viewsWare[parentView].depViews[fieldName][0]
  view = $el(depViewName)

  return if view.is(':visible')
  $(".complex-field").hide()

  view.show ->
    initOSM e for e in view.find(".osMap")

    if depViewName.match(/contractor_partner-view/)
      table = view.find("table#contractor_partnerTable")
      kase = global.viewsWare["case-form"].knockVM
      svc = _.find(
              kase.servicesReference(),
              (svc) -> svc.view is parentView)

      unless table.hasClass("dataTable")
        mkDataTable(table)
        table.data("dataTable", table)
        table.on "click.datatable", "tr", ->
          name = this.children[0].innerText
          city = this.children[1].innerText
          addr = this.children[2].innerText
          svc.contractor_partner(name)
          svc.contractor_address("#{city}, #{addr}")

      table = table.data("dataTable")
      fields = "name,cityRu,addrDeFacto,phone1,workingTime"
      select = "cityRu == #{kase.cityLocal()}" # , isActive == 1, isDealer == 0"
      $.getJSON "/all/partner?fields=#{fields}&select=#{select}", (objs) ->
        rows = for p in objs
          [p.name,
           p.cityRu,
           p.addrDeFacto,
           p.phone1,
           p.workingTime]
        table.fnClearTable()
        table.fnAddData(rows)


this.hideComplex = ->
  $(".complex-field").hide()
  $(".default-complex-field").show()


initOSM = (el) ->
  return if $(el).hasClass("olMap")

  osmap = new OpenLayers.Map(el.id)
  osmap.addLayer(new OpenLayers.Layer.OSM())
  osmap.setCenter(
    new OpenLayers.LonLat(37.617874,55.757549)
      .transform(
        new OpenLayers.Projection("EPSG:4326"),
        osmap.getProjectionObject()
      ),
    16 # Zoom level
  )
  $(el).data("osmap", osmap)


# Dispatch on some picker type
#
# Available picks:
#
# - vinFiller
this.doPick = (pickType, args, el) ->
  pickers =

    callPlease: (modelName) ->
      bb = global.viewsWare["call-form"].bbInstance
      number = bb.get(modelName)
      global.avayaPhone && global.avayaPhone.call(number)

    nominatimPicker: (fieldName, el) ->
      addr = $(el).parent().prev().val()
      $.getJSON("/nominatim?addr=#{addr}", (res) ->
        if res.length > 0
          form = $(el).parents("form")
          osmap = form.find(".olMap")
          res1 = JSON.parse(res)
          osmap.data().osmap.setCenter(
            new OpenLayers.LonLat(res1[0].lon, res1[0].lat)
              .transform(
                new OpenLayers.Projection("EPSG:4326"),
                new OpenLayers.Projection("EPSG:900913")
              )
            , 16))
  pickers[pickType](args, el)

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

################################################################################
# utils
this.toUnix = (d) -> Math.round(d.getTime() / 1000)
