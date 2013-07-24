define [ "utils"
       , "map"
       , "model/main"
       , "sync/dipq"
       , "text!tpl/screens/partnersSearch.html"
       ], (utils, map, m, sync, tpl) ->

  storeKey = 'partnersSearch'
  subName = (fld, model, id) ->
    if id
      "search_#{model}:#{id}_#{fld}"
    else
      "search_#{model}_#{fld}"

  open = (prm) -> window.open("/#partnersSearch/#{prm}", "_blank")

  model =
    name: "partnerSearch"
    title: "Экран поиска партнеров"
    fields: [
      { name: "search"
      , meta: { label: "Поиск" }
      },
      { name: "city"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "DealerCities"
          label: "Город"
      },
      { name: "make"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "CarMakers"
          label: "Марка"
      },
      { name: "services"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Services"
          label: "Услуги"
      },
      { name: "priority2"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Priorities"
          dictionaryType: "ComputedDict"
          bounded: true
          label: "ПБГ"
      },
      { name: "priority3"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Priorities"
          dictionaryType: "ComputedDict"
          bounded: true
          label: "ПНГ"
      },
      { name: "isDealer"
      , type: "checkbox"
      , meta: { label: "Дилер" }
      },
      { name: "mobilePartner"
      , type: "checkbox"
      , meta: { label: "Мобильный партнер" }
      },
      { name: "workNow"
      , type: "checkbox"
      , meta: { label: "Работают сейчас" }
      }
      ]

  # fh is just mapping from field name to field
  fh = {}
  fh[f.name] = f for f in model.fields

  mkPartials = (ps) ->
    $("<script class='partial' id='#{k}'>").html(v)[0].outerHTML for k,v of ps

  partialize = (ps) -> mkPartials(ps).join('')

  partnerPopupTpl = $("#partner-popup-template").html()
  txt = $("#text-field-template").html()
  md  = $("#dictionary-many-field-template").html()
  cb  = $("#checkbox-field-template").html()
  srch = Mustache.render txt, fh['search']
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
    kaseKVM = m.buildKVM global.models['case'],  {fetched: kase}
    srvKVM  = m.buildKVM global.models[srvName], {fetched: data}
    kvm['fromCase'] = true
    kvm['city'](kaseKVM.city())
    kvm['make'](kaseKVM.car_make())
    kvm['services'](srvName)
    kvm['isDealer'](ctx['field'].split('_')[0] != 'contractor')
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
    </ul>
    """
    kvm['caseCoords'] = map.lonlatFromShortString kaseKVM.caseAddress_coords()
    kvm['selectPartner'] = (partner, ev) ->
      kvm['selectedPartner'](partner.id)
      # Highlight partner blip on map
      $("#map").trigger "drawpartners"
      global.pubSub.pub subName(ctx.field, id), partner

  loadContext = (kvm, args) ->
    return unless args?.model
    return unless localStorage['partnersSearch']
    ctx = JSON.parse localStorage['partnersSearch']
    switch args.model
      when "case"
        setupCase kvm, ctx
      when "call"
        kvm['city'](ctx.city)
        kvm['make'](ctx.carMake)

    # cleanup localstore
    localStorage.removeItem 'partnersSearch'

  resizeResults = ->
    t = $("#search-result").offset().top
    w = $(window).height()
    $("#search-result").height(w-t-10)

    t = $("#map").offset().top
    $("#map").height(w-t-5)

  # Obtain coordinates of city (internal value) or return null in case
  # of failure
  cityToCoords = (city, coords) ->
    fixed_city = global.dictValueCache.DealerCities[city]
    $.getJSON map.geoQuery(fixed_city), (res) ->
      if res.length > 0
        coords = new OpenLayers.LonLat res[0].lon, res[0].lat
      else
        coords = null

  # Bind cityPlaces observableArray to list of places of currently
  # selected cities in `city`. A place is an object with fields
  # `coords` (OpenLayers coordinate) and `bounds` (bounding box as an
  # array), which can be displayed on the map (centered on coords with
  # some zoom level, or simply zoomed to fit bounds).
  bindCityPlaces = (kvm) ->
    kvm["city"].subscribe (newCities) ->
      chunks = _.reject newCities.split(","), _.isEmpty
      kvm["cityPlacesExpected"] = chunks.length
      kvm["cityPlaces"].removeAll()
      for c in chunks
        do (c) ->
          fixed_city = global.dictValueCache.DealerCities[c]
          $.getJSON map.geoQuery(fixed_city), (res) ->
            if res.length > 0
              # TODO Remove this hack (#837) after Cities dict is
              # implemented properly
              if c == "Moskva"
                place = Moscow
              else if c == "Sankt-Peterburg"
                place = Petersburg
              else
                # Decode Nominatim bounding box using proper argument order
                bb = res[0].boundingbox
                place =
                  coords: new OpenLayers.LonLat res[0].lon, res[0].lat
                  bounds: new OpenLayers.Bounds bb[2], bb[0], bb[3], bb[1]
              kvm["cityPlaces"].push place

  # Cut off everything beyond The Wall
  Moscow =
    coords: new OpenLayers.LonLat(37.617874, 55.757549)
    bounds: new OpenLayers.Bounds(
      37.2, 55.5,
      37.9674301147461, 56.0212249755859)

  Petersburg =
    coords: new OpenLayers.LonLat(30.312458, 59.943168)
    bounds: new OpenLayers.Bounds(
      29.4298095703125, 59.6337814331055,
      30.7591361999512, 60.2427024841309)

  # Install OpenLayers map in #map element, setup repositioning hooks
  setupMap = (kvm) ->
    osmap = new OpenLayers.Map("map")
    osmap.addLayer(new OpenLayers.Layer.OSM())
    osmap.zoomToMaxExtent()

    # Crash site blip
    if kvm["caseCoords"]?
      coords = kvm["caseCoords"].clone().transform(map.wsgProj, map.osmProj)
      ico = new OpenLayers.Icon(map.carIcon, map.iconSize)
      backLayer = new OpenLayers.Layer.Markers("back")
      osmap.addLayer backLayer
      backLayer.addMarker(
        new OpenLayers.Marker coords, ico)

    getFactAddress = (addrs) ->
      utils.keyedJsonValue addrs, "fact"
      
    getPhone = (phones) ->
      if kvm["isDealer"]()
        key = "serv"
      else
        key = "disp"
      utils.keyedJsonValue phones, key          
          
    # Draw partners on map
    redrawPartners = (newPartners) ->
      partnerLayer = map.reinstallMarkers(osmap, "partners")
      osmap.raiseLayer partnerLayer, -1
      for p in newPartners
        do (p) ->
          # Pick partner icon
          if p.ismobile
            ico = map.towIcon
          else
            if p.isdealer
              ico = map.dealerIcon
            else
              ico = map.partnerIcon

          if p.id == kvm["selectedPartner"]()
            ico = map.hlIconName(ico)
            
          coords = new OpenLayers.LonLat p.st_x, p.st_y
          # Add blip to map
          mark = new OpenLayers.Marker(
            coords.transform(map.wsgProj, map.osmProj),
            new OpenLayers.Icon(ico, map.iconSize))

          # Bind info popup to blip click event
          mark.events.register "click", mark, () ->
            # Format JSON fields
            extra_ctx =
              address: getFactAddress p.addrs
              phone: getPhone p.phones
            ctx = _.extend p, extra_ctx
            popup = new OpenLayers.Popup.FramedCloud(
              p.id, mark.lonlat,
              new OpenLayers.Size(200, 200),
              Mustache.render(partnerPopupTpl, p),              
              null, true)
            osmap.addPopup popup

            # Provide select button if came from case
            if kvm["fromCase"]
              $(popup.div).find(".btn").click (e) ->
                kvm["selectPartner"](p, e)
                $("#map").trigger "drawpartners"
            # Otherwise just delete the button
            else
              $(popup.div).find(".btn-div").remove()
          
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

    # Fit places into the viewport
    reposition = (places) ->
      # Show one place or even fall back to default place
      if places.length <= 1
        # Starting with no places || no location or city set on case.
        if _.isEmpty places || _.isUndefined places[0].coords
          place = Moscow
        else
          place = places[0]
        # Select zoom level from bounds
        if place.bounds?
          osmap.zoomToExtent(
            place.bounds.clone().transform(map.wsgProj, map.osmProj), true)
        # Then recenter on the very place for better positioning (your
        # experience may vary)
        osmap.setCenter(
          place.coords.clone().transform(map.wsgProj, map.osmProj))

      # Fit several places in viewport
      else
        # Pick first bounded place
        place = _.find places, (p) -> !_.isUndefined p.bounds
        bounds = place.bounds.clone()
        # Closefitting hides encircled cities at times
        closefit = false
        # Grow to include all other places
        for p in places
          if p.bounds?
            bounds.extend p.bounds
          else
            # A place without bounds is usually a crash site pin.
            # Enabling closefitting after bounds have been extended with
            # single coordinate pin produces visually more appealing
            # results.
            if places.length == 2
              closefit = true
            bounds.extend p.coords

        osmap.zoomToExtent(
          bounds.transform(map.wsgProj, map.osmProj), closefit)

    # Refit map when more cities are selected
    kvm["cityPlaces"].subscribe (newCityPlaces) ->
      # Refit map only if all cities have been fetched (prevents
      # blinking Moscow syndrome)
      return if kvm["cityPlacesExpected"] > kvm["cityPlaces"]().length
      if kvm["caseCoords"]?
        reposition [coords: kvm["caseCoords"]].concat newCityPlaces
      else
        reposition newCityPlaces

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
    $('body').css('padding-top', '0px')
    $(".navbar").hide()

    kvm = m.buildKVM(model, "partnersSearch-content")
    q = new sync.DipQueue(kvm, model)
    kvm._meta.q = q
    kvm['choosenSort'] = ko.observableArray(["priority3"])
    kvm['searchResults'] = ko.observable()
    kvm['searchH'] = ko.computed ->
      s = kvm['searchResults']()
      return [] unless s
      r = {}
      for v in s
        r[v.id] ?= v
        r[v.id]['services'] ?= []
        r[v.id]['services'].push
          label    : srvLab v.servicename
          name     : v.servicename
          priority2: v.priority2
          priority3: v.priority3
      r

    kvm['searchProcessed'] = ko.computed ->
      sort = kvm['choosenSort']()[0]
      srvs = kvm['servicesLocals']()
      s = for k, v of kvm['searchH']()
        v.services = _.sortBy v.services, (v) -> [v.priority2, v.priority3]
        v
      return s if _.isEmpty srvs
      _.sortBy s, (v) ->
        parseInt (_.find v.services, (s) -> s.name == srvs[0].value)?[sort]
    kvm['selectedPartner'] = ko.observable(NaN)

    kvm["cityPlaces"] = ko.observableArray []
    bindCityPlaces kvm

    loadContext kvm, args

    # responsive web design damn it, can't use heigth: Nvw because of header
    resizeResults()
    $(window).resize resizeResults

    setupMap kvm
    
    kvm['caseInfo'] ?= ""
    ko.applyBindings kvm, $('#partnersSearch-content')[0]
    $("#case-info").popover { template: $("#custom-popover").html() }
    q.search()

    # FIXME Remove this
    $("#partnersSearch-content").data "kvm", kvm

  # key to retrieve data for partnerSearch screen from localstore
  storeKey: storeKey
  # key that service should subscribe to get data back
  subName: subName
  open: open
  template: tpl
  partials: partialize
    search        : srch
    city          : city
    make          : make
    dealer        : dlr
    mobilePartner : mbp
    workNow       : wn
    services      : srvs
    priority2     : pr2
    priority3     : pr3