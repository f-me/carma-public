define [ "utils"
       , "map"
       , "model/main"
       , "sync/dipq"
       , "dictionaries"
       , "lib/time"
       , "partnerCancel"
       , "screens/partnersSearch/models"
       , "sync/metaq"
       , "text!tpl/screens/partnersSearch.html"
       , "text!tpl/partials/partnersSearch.html"
       ], ( utils
          , map
          , m
          , sync
          , dict
          , time
          , partnerCancel
          , models
          , metaq
          , tpl
          , partials) ->

  model = models.PartnerSearch

  storeKey = 'partnersSearch'
  subName = (fld, model, id) ->
    if id
      "search_#{model}:#{id}_#{fld}"
    else
      "search_#{model}_#{fld}"

  open = (prm) -> window.open("/#partnersSearch/#{prm}", "_blank")

  # fh is just mapping from field name to field
  fh = {}
  fh[f.name] = f for f in model.fields

  mkPartials = (ps) ->
    $("<script class='partial' id='#{k}'>").html(v)[0].outerHTML for k,v of ps

  partialize = (ps) -> mkPartials(ps).join('')

  md  = $(partials).html()
  cb  = $("#checkbox-field-template").html()
  city = Mustache.render md,  fh['city']
  make = Mustache.render md,  fh['make']
  srvs = Mustache.render md,  fh['services']
  pr2  = Mustache.render md,  fh['priority2']
  pr3  = Mustache.render md,  fh['priority3']
  dlr  = Mustache.render cb,  fh['isDealer']
  mbp  = Mustache.render cb,  fh['mobilePartner']
  wn   = Mustache.render cb,  fh['workNow']

  srvLab = (val) -> window.global.dictValueCache.Services[val] || val

  # Add some of case data to screen kvm
  setupCase = (kvm, ctx) ->
    kase = ctx['case'].data
    {id, data} = ctx['service']
    srvName = id.split(':')[0]
    kaseKVM = m.buildKVM global.model('case'),  {fetched: kase}
    srvKVM  = m.buildKVM global.model(srvName), {fetched: data}
    kvm['fromCase'] = true
    kvm['city'](kaseKVM.city())
    kvm['make'](kaseKVM.car_make())
    kvm['field'] = ctx['field']

    pid = parseInt srvKVM["#{ctx['field']}Id"]()?.split(":")[1]
    if _.isNumber(pid) && !_.isNaN(pid)
      kvm['selectedPartner'] pid
    else
      kvm['selectedPartner'] null

    # Set isDealer flag depending on what field we came from
    unless ctx['field'].split('_')[0] == 'contractor'
      kvm['isDealer'](true)
    else
      kvm['services'](srvName)
    kvm['isDealerDisabled'](true)
    kvm['caseInfo'] = """
    <ul class='unstyled'>
      <li>
        <b>Кто звонил:</b>
        #{kaseKVM.contact_name() || ''} #{kaseKVM.contact_phone1() || ''}
      </li>
      <li> <b>Номер кеса:</b> #{kaseKVM.id() || ''} </li>
      <li> <b>Адрес кейса:</b> #{kaseKVM.caseAddress_address() || ''}</li>
      <li> <b>Название программы: </b> #{kaseKVM.programLocal() || ''} </li>
      <li> <b> Марка: </b> #{kaseKVM.car_makeLocal() || ''}</li>
      <li> <b> Модель: </b> #{kaseKVM.car_modelLocal() || ''}</li>
      <li> <b> Госномер: </b> #{kaseKVM.car_plateNum() || ''}</li>
      <li> <b> Цвет: </b> #{kaseKVM.car_colorLocal() || ''}</li>
      <li> <b> VIN:</b> #{kaseKVM.car_vin() || ''}</li>
      <li> <b> Тип оплаты:</b> #{srvKVM.payTypeLocal() || ''}</li>
      <li> <b> Клиент/Доверенное лицо будет сопровождать автомобиль:</b>
      #{if srvKVM.companion?() then '✓' else '' }</li>
    </ul>
    """
    kvm['coords'] kaseKVM.caseAddress_coords()
    kvm['address'] kaseKVM.caseAddress_address()

    selectPartner = (kvm, partner) ->
      if _.isNull partner
        # Deselect partner
        kvm['selectedPartner'](null)
        global.pubSub.pub subName(ctx.field, id),
          name: ''
          addrDeFacto: ''
          id: ''
          distanceFormatted: ''
      else
        kvm['selectedPartner'](partner.id())
        # Highlight partner blip on map
        $("#map").trigger "drawpartners"
        if kvm['field'].split('_')[0] == 'contractor'
          partner['addrDeFacto'] utils.getKeyedJsonValue partner.addrs(), 'fact'
        else
          a = partner['addrDeFacto']
          a(utils.getKeyedJsonValue partner.addrs(), 'serv')
          a(utils.getKeyedJsonValue partner.addrs(), 'fact') unless a()
        global.pubSub.pub subName(ctx.field, id), partner._meta.q.toRawObj()

    kvm['selectPartner'] = (partner, ev) ->
      selected = kvm['selectedPartner']()
      # don't select same partner twice
      return if selected == partner?.id
      return if partner?.isfree == false
      if _.isNull selected
        selectPartner(kvm, partner)
      else
        partnerCancel.setup "partner:#{selected}", ctx.service.id, ctx.case.id
        partnerCancel.onSave ->
          selectPartner(kvm, partner)
          $("#map").trigger "drawpartners"

    kvm['showPartnerCancelDialog'] = (partner, ev) -> kvm['selectPartner'](null)

  loadContext = (kvm, args) ->
    s = localStorage['partnersSearch']
    ctx = JSON.parse s if s
    # By default, map is unclickable
    kvm['mapClickable'] = false
    $("#case-info").css 'visibility', 'hidden'
    switch args?.model
      when "case"
        $("#case-info").css 'visibility', 'visible'
        return unless args?.model and s
        setupCase kvm, ctx
      when "call"
        return unless args?.model and s
        kvm['city'](ctx.city)
        kvm['make'](ctx.carMake)
        kvm['isDealer'](true)

        kvm['coords'] ctx.coords
        kvm['address'] ctx.address
        # Allow resetting address & city of case
        kvm['mapClickable'] = true

        # Show heads-up address search field
        $("#map-search-overlay").show()

        # Asynchronously push kvm updates to pubsub for call model
        setupPusher = (field) ->
          kvm[field].subscribe (val) ->
            n = subName field, 'call', ctx.id
            global.pubSub.pub n, kvm[field]()
        setupPusher 'coords'
        setupPusher 'address'
      when "mobile"
        kvm['mobilePartner'](true)

    # cleanup localstore
    localStorage.removeItem 'partnersSearch'

  resizeResults = ->
    t = $("#search-result").offset().top
    w = $(window).height()
    $("#search-result").height(w-t-10)

    t = $("#map").offset().top
    $("#map").height(w-t-5)

    # Position address search field over the map in right top corner
    ar_w = $("#map-search-overlay").width()
    map_r = $("#map").offset().left + $("#map").width()
    $("#map-search-overlay").offset
      top: t + 5
      left: map_r - ar_w - 10

  # Bind cityPlaces observableArray to list of places of currently
  # selected cities in `city`. A place is an object with fields
  # `coords` (OpenLayers coordinate) and `bounds` (bounding box as an
  # array), which can be displayed on the map (centered on coords with
  # some zoom level, or simply zoomed to fit bounds).
  bindCityPlaces = (kvm) ->
    kvm["city"].subscribe (newCities) ->
      return unless newCities?
      chunks = _.reject newCities.split(","), _.isEmpty
      kvm["cityPlacesExpected"] = chunks.length
      kvm["cityPlaces"].removeAll()
      for c in chunks
        do (c) ->
          fixed_city = global.dictValueCache.DealerCities[c]
          $.getJSON map.geoQuery(fixed_city), (res) ->
            if res.length > 0
              place = map.buildCityPlace res
              kvm["cityPlaces"].push place

  # Format addrs field for partner info template
  getFactAddress = (addrs) ->
    if addrs?.length > 0
      utils.getKeyedJsonValue addrs, "fact"

  # Format phones field for partner info template
  getPhone = (kvm, phones) ->
    if phones?.length > 0
      if kvm["isDealer"]()
        key = "serv"
      else
        key = "disp"
      utils.getKeyedJsonValue phones, key

  # Install OpenLayers map in #map element, setup repositioning hooks
  setupMap = (kvm) ->
    osmap = new OpenLayers.Map("map")
    osmap.addLayer(new OpenLayers.Layer.OSM())
    osmap.zoomToMaxExtent()

    # Crash site blip initial position
    if kvm["coords"]()?
      coords =
        (map.lonlatFromShortString kvm["coords"]())
        .clone()
        .transform(map.wsgProj, map.osmProj)
      map.currentBlip osmap, coords, "car"

    if kvm["mapClickable"]
      # Crash site relocation on map click
      osmap.events.register "click", osmap, (e) ->
        raw_coords = osmap.getLonLatFromViewPortPx(e.xy)
        coords = raw_coords.clone().transform(map.osmProj, map.wsgProj)

        map.spliceCoords coords, kvm,
          coord_field: "coords"
          osmap: osmap
          current_blip_type: "car"
          addr_field: "address"
          # city_field: "city"

        kvm._meta.q._search()

      # Address search box handlers
      search = $("#map-search-field")
      search_button = $("#map-search-button")

      # Start search string with currently selected city
      if _.isEmpty kvm['address']()
        city = kvm['cityLocals']()[0]?.label
        search.val(city)

      search.keypress (e) ->
        if e.which == 13
          search_button.trigger "click"

      search_button.click () ->
        map.spliceAddress search.val(), kvm,
          coord_field: "coords"
          osmap: osmap
          current_blip_type: "car"
          city_field: "city"

        kvm._meta.q._search()

    # Draw partners on map
    redrawPartners = (newPartners) ->
      partnerLayer = map.reinstallMarkers(osmap, "partners")
      osmap.raiseLayer partnerLayer, -1
      for p in newPartners
        do (p) ->
          # Pick partner icon
          if p.ismobile()
            if p.isfree()
              ico = map.towIcon
            else
              ico = map.busyTowIcon
          else
            if p.isdealer()
              ico = map.dealerIcon
            else
              ico = map.partnerIcon

          if p.id() == kvm["selectedPartner"]()
            ico = map.hlIconName(ico)

          coords = new OpenLayers.LonLat p.st_x(), p.st_y()
          # Add blip to map
          mark = new OpenLayers.Marker(
            coords.transform(map.wsgProj, map.osmProj),
            new OpenLayers.Icon(ico, map.iconSize))

          # Bind info popup to blip click event
          mark.events.register "click", mark, () ->
            partner_popup = $ $("#partner-" + p.id() + "-info").clone().html()
            partner_popup.find(".full-info-link").hide()
            popup = new OpenLayers.Popup.FramedCloud(
              p.id(), mark.lonlat,
              new OpenLayers.Size(200, 200),
              partner_popup.html(),
              null, true)
            osmap.addPopup popup

            # Provide select button if came from case
            if kvm["fromCase"]
              $(popup.div).find(".btn-div").show()
              $(popup.div).find(".select-btn").click (e) ->
                kvm["selectPartner"](p, e)
                $("#map").trigger "drawpartners"
              popup.updateSize()

          partnerLayer.addMarker(mark)

    # Redo the search when dragging or zooming
    osmap.events.register "moveend", osmap, () ->
      # Calculate new bounding box
      bounds = osmap.getExtent()
      pts = bounds.toArray()
      a = new OpenLayers.LonLat(pts[0], pts[1])
      b = new OpenLayers.LonLat(pts[2], pts[3])
      a.transform(map.osmProj, map.wsgProj)
      b.transform(map.osmProj, map.wsgProj)
      kvm["mapA"] = a
      kvm["mapB"] = b
      kvm._meta.q._search()

    # Refit map when more cities are selected
    kvm["cityPlaces"].subscribe (newCityPlaces) ->
      # Refit map only if all cities have been fetched (prevents
      # blinking Moscow syndrome)
      return if kvm["cityPlacesExpected"] > kvm["cityPlaces"]().length
      if kvm["coords"]()?
        coords = map.lonlatFromShortString kvm["coords"]()
        map.fitPlaces osmap, [coords: coords].concat newCityPlaces
      else
        map.fitPlaces osmap, newCityPlaces

    # Set initial position
    kvm["cityPlaces"].valueHasMutated()

    # Update partner blips when redoing search
    kvm["searchProcessed"].subscribe redrawPartners

    # Provide way to force partner blips re-rendering, hiding mutation
    # primitive
    $("#map").on "drawpartners", () ->
      redrawPartners kvm["searchProcessed"]()

    # Force first map data rendering
    osmap.events.triggerEvent("moveend")

    $("#map").data("osmap", osmap)

  constructor: (view, args) ->
    # remove padding so blank space after removing navbar can be used
    if args?
      $('body').css('padding-top', '0px')
      $(".navbar").hide()

    kvm = m.buildKVM(model, "partnersSearch-content")
    q = new sync.DipQueue(kvm, model)
    kvm._meta.q = q
    kvm['id'](1) # just to make disabled observables work
    kvm['choosenSort'] = ko.observableArray(["priority2"])
    kvm['searchResults'] = ko.observable()
    kvm['searchH'] = ko.computed ->
      s = kvm['searchResults']()
      return [] unless s
      ms = for i in s
        k = m.buildKVM models.SearchResults, {fetched: i, models: models}
        k._meta.q = new metaq(k, models.SearchResults)

        k['distanceFormatted'] = ko.computed ->
          utils.formatDistance k['distance']()

        k['factAddr'] = ko.computed ->
          (_.filter k['addrs'](), ({key}) -> key == 'fact')[0]?.value || ''

        k['makesLocalsString'] = ko.computed ->
          (_.pluck k['makesLocals'](), 'label').join(',')

        k['coords'] = ko.computed -> "#{k['st_x']()},#{k['st_y']()}"

        for nested in k['servicesNested']()
          do (nested) ->
            nested['showStr'] = ko.computed ->
              show  = "<span class='label label-info'>
                       #{srvLab nested.servicenameLocal()}
                       </span>"
              if nested.priority2()
                show += " <span class='label label-important'>
                          ПБГ: #{nested.priority2()}
                          </span>"
              if nested.priority3()
                show += " <span class='label label-warning'>
                          ПБЗ: #{nested.priority3()}
                          </span>"
              show

        k
      return ms

    kvm['searchA'] = ko.computed -> _.values kvm['searchH']()

    kvm["searchK"] = ko.computed(->kvm["search"]()).extend { throttle: 300 }

    kvm['searchProcessed'] = ko.sorted
        kvms: kvm['searchA']

        filters:
          searchq:  (v) ->
            return true unless kvm['searchK']()
            utils.kvmCheckMatch kvm['searchK'](), v
          workNow: (v) ->
            return true unless kvm['workNow']()
            k = if v.isdealer() then 'serv' else 'disp'
            times = _.reduce (_.filter v.phonesObjects(), (p) -> p.key() == k),
                             ((a, p) -> "#{a}, #{(p.note() or '')}"),
                             ''
            time.isWorkingNow time.parseWorkTimes(times)

        sorters:
          only: (v) ->
            sort = kvm['choosenSort']()[0]
            srvs = kvm['servicesLocals']()
            unless _.isEmpty srvs
              parseInt (_.find v.servicesNested(),
                       (s) -> s.servicename() == srvs[0].value)?[sort]?()

    kvm['searchProcessed'].set_sorter('only')
    kvm['searchProcessed'].change_filters ['searchq', 'workNow']
    kvm['selectedPartner'] = ko.observable(null)

    kvm["cityPlaces"] = ko.observableArray []
    bindCityPlaces kvm

    loadContext kvm, args

    # responsive web design damn it, can't use heigth: Nvw because of header
    resizeResults()
    $(window).resize resizeResults

    kvm['caseInfo'] ?= ""
    ko.applyBindings kvm, $('#partnersSearch-content')[0]
    $("#case-info").popover { template: $("#custom-popover").html() }

    setupMap kvm

    # Store a hook to KVM for debugging
    $("#partnersSearch-content").data "kvm", kvm

  # key to retrieve data for partnerSearch screen from localstore
  storeKey: storeKey
  # key that service should subscribe to get data back
  subName: subName
  open: open
  template: tpl
  partials: partialize
    city          : city
    make          : make
    dealer        : dlr
    mobilePartner : mbp
    workNow       : wn
    services      : srvs
    priority2     : pr2
    priority3     : pr3
