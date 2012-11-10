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
        destructor: removeRKCScreen
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
  editSms     :      -> renderScreen("editSms")

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

    isDealerView = depViewName.match(/towDealer_partner-view/)
    isPartnerView = depViewName.match(/contractor_partner-view/)
    if isDealerView or isPartnerView
      initPartnerTables view, parentView


this.hideComplex = ->
  $(".complex-field").hide()
  $(".default-complex-field").show()


# Build readable address from reverse Nominatim JSON response
this.buildReverseAddress = (res) ->
  addr = (res.address.road || res.address.pedestrian)

  if (_.isUndefined(res.address.house_number))
     return addr
  else
     return addr +  ", " + res.address.house_number

this.clearMarkers = (markers) ->
  markers.removeMarker(m) for m in markers.markers


# Move the car crash blip on the map
#
# xy is from original click event
this.carBlip = (osmap, xy) ->
  ico = new OpenLayers.Icon("/s/img/car-icon.png")
  markers = osmap.getLayersByName("Car")[0]
  clearMarkers(markers)
  markers.addMarker(
    new OpenLayers.Marker(osmap.getLonLatFromViewPortPx(xy), ico))


# Render list of partners on the map
#
# partners is a list of [id, lon, lat]
this.partnerBlips = (osmap, partners) ->
  ico = new OpenLayers.Icon("/s/img/tow-icon.png")
  markers = osmap.getLayersByName("Partners")[0]

  for blip in partners
     do (blip) ->
       coords = new OpenLayers.LonLat(blip[1], blip[2])
                    .transform(new OpenLayers.Projection("EPSG:4326"),
                               new OpenLayers.Projection("EPSG:900913"))
       markers.addMarker(new OpenLayers.Marker(coords, ico))
  

initOSM = (el) ->
  return if $(el).hasClass("olMap")

  osmap = new OpenLayers.Map(el.id)
  clicker = new OpenLayers.Handler.Click(el.id)

  osmap.addLayer(new OpenLayers.Layer.OSM())
  osmap.setCenter(
    new OpenLayers.LonLat(37.617874,55.757549)
      .transform(
        new OpenLayers.Projection("EPSG:4326"),
        osmap.getProjectionObject()
      ),
    16 # Zoom level
  )
  nominatimRevQuery = 
      "http://nominatim.openstreetmap.org/reverse.php?format=json&accept-language=ru-RU,ru&"

  markers = new OpenLayers.Layer.Markers("Car")
  osmap.addLayer(markers)

  partners = new OpenLayers.Layer.Markers("Partners")
  osmap.addLayer(partners)

  if ($(el).data("target-addr"))
    osmap.events.register("click", osmap, (e) ->
      coords = osmap.getLonLatFromViewPortPx(e.xy)
               .transform(new OpenLayers.Projection("EPSG:900913"),
                          new OpenLayers.Projection("EPSG:4326"))
      # Reverse geocode on clicks and write new address to "maptarget" field
      $.getJSON(nominatimRevQuery + "lon=#{coords.lon}&lat=#{coords.lat}", (res) ->
        $.getJSON("/geo/partners/#{coords.lon},#{coords.lat}/0.5", (pres) ->
          addr = buildReverseAddress(res)

          addr_field = $(el).data("target-addr")
          city_field = $(el).data("target-city")
          global.viewsWare['case-form'].knockVM[addr_field](addr)
          # global.viewsWare['case-form'].knockVM[city_field](res.address.city)

          carBlip(osmap, e.xy)
          partnerBlips(osmap, pres)))
    )

  $(el).data("osmap", osmap)


# Dispatch on some picker type
#
# Available picks:
#
# - vinFiller
this.doPick = (pickType, args, evt) ->
  pickers =

    callPlease: (modelName) ->
      bb = global.viewsWare["call-form"].bbInstance
      number = bb.get(modelName)
      global.avayaPhone && global.avayaPhone.call(number)

    nominatimPicker: (fieldName, evt) ->
      el = evt.target
      addr = $(el).parents('.input-append')
                  .children("input[name=#{fieldName}]")
                  .val()
      nominatimQuery = 
          "http://nominatim.openstreetmap.org/search?format=json&accept-language=ru-RU,ru&q="
      $.getJSON(nominatimQuery+"#{addr}", (res) ->
        if res.length > 0
          form = $(el).parents("form")
          osmap = form.find(".olMap")
          return if res.length == 0
          osmap.data().osmap.setCenter(
            new OpenLayers.LonLat(res[0].lon, res[0].lat)
              .transform(
                new OpenLayers.Projection("EPSG:4326"),
                new OpenLayers.Projection("EPSG:900913")
              )
            , 16)
          carBlip(osmap, osmap.data("osmap").getCenter()))
  pickers[pickType](args, evt)

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
