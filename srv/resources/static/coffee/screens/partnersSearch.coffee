define [ "utils"
       , "map"
       , "model/main"
       , "sync/dipq"
       , "dictionaries"
       , "lib/time"
       , "partnerCancel"
       , "text!tpl/screens/partnersSearch.html"
       , "text!tpl/partials/partnersSearch.html"
       ], (utils, map, m, sync, dict, time, partnerCancel, tpl, partials) ->

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
      , meta: { label: "Поиск", nosearch: true }
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
          bounded: false
          label: "ПБГ"
      },
      { name: "priority3"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Priorities"
          dictionaryType: "ComputedDict"
          bounded: false
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
      , meta: { label: "Работают сейчас", nosearch: true}
      }
      ]

  # fh is just mapping from field name to field
  fh = {}
  fh[f.name] = f for f in model.fields

  mkPartials = (ps) ->
    $("<script class='partial' id='#{k}'>").html(v)[0].outerHTML for k,v of ps

  partialize = (ps) -> mkPartials(ps).join('')

  partnerPopupTpl = $("#partner-popup-template").html()
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
    kaseKVM = m.buildKVM global.models['case'],  {fetched: kase}
    srvKVM  = m.buildKVM global.models[srvName], {fetched: data}
    kvm['fromCase'] = true
    kvm['city'](kaseKVM.city())
    kvm['make'](kaseKVM.car_make())
    kvm['selectedPartner'](srvKVM["#{ctx['field']}Id"]?())
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
    </ul>
    """
    kvm['caseCoords'] = map.lonlatFromShortString kaseKVM.caseAddress_coords()

    selectPartner = (kvm, partner) ->
      if _.isNull partner
        kvm['selectedPartner'](null)
        global.pubSub.pub subName(ctx.field, id),
          name: ''
          addrdefacto: ''
          id: ''
      else
        kvm['selectedPartner'](partner.id)
        # Highlight partner blip on map
        $("#map").trigger "drawpartners"
        global.pubSub.pub subName(ctx.field, id), partner

    kvm['selectPartner'] = (partner, ev) ->
      selected = kvm['selectedPartner']()
      # don't select same partner twice
      return if selected == partner?.id
      if _.isNull selected
        selectPartner(kvm, partner)
      else
        partnerCancel.setup selected, ctx.service.id, ctx.case.id
        partnerCancel.onSave -> selectPartner(kvm, partner)

    kvm['showPartnerCancelDialog'] = (partner, ev) -> kvm['selectPartner'](null)

  loadContext = (kvm, args) ->
    s = localStorage['partnersSearch']
    ctx = JSON.parse s if s
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

    # Crash site blip
    if kvm["caseCoords"]?
      coords = kvm["caseCoords"].clone().transform(map.wsgProj, map.osmProj)
      ico = new OpenLayers.Icon(map.carIcon, map.iconSize)
      backLayer = new OpenLayers.Layer.Markers("back")
      osmap.addLayer backLayer
      backLayer.addMarker(
        new OpenLayers.Marker coords, ico)

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
              phone: getPhone kvm, p.phones
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

    # Refit map when more cities are selected
    kvm["cityPlaces"].subscribe (newCityPlaces) ->
      # Refit map only if all cities have been fetched (prevents
      # blinking Moscow syndrome)
      return if kvm["cityPlacesExpected"] > kvm["cityPlaces"]().length
      if kvm["caseCoords"]?
        map.fitPlaces osmap, [coords: kvm["caseCoords"]].concat newCityPlacess
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

  # deep check that anything in @val@ has @q@
  checkMatch = (q, val) ->
    if _.isArray val
      _.any val, (a) -> checkMatch(q, a)
    else if _.isObject val
      _.any (checkMatch(q, v) for k, v of val), _.identity
    else
      !!~String(val).toLowerCase().indexOf(q.toLowerCase())
  window.checkMatch = checkMatch

  constructor: (view, args) ->
    # remove padding so blank space after removing navbar can be used
    $('body').css('padding-top', '0px')
    $(".navbar").hide()

    PhoneTypes   = new dict.dicts['LocalDict'] {dict: 'PhoneTypes'}
    AddressTypes = new dict.dicts['LocalDict'] {dict: 'AddressTypes'}
    EmailTypes   = new dict.dicts['LocalDict'] {dict: 'EmailTypes'}
    DealerCities = new dict.dicts['LocalDict'] {dict: 'DealerCities'}
    CarMakers    = new dict.dicts['LocalDict'] {dict: 'CarMakers'}

    kvm = m.buildKVM(model, "partnersSearch-content")
    q = new sync.DipQueue(kvm, model)
    kvm._meta.q = q
    kvm['id'](1) # just to make disabled observables work
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
        r[v.id]['cityLocal'] = DealerCities.getLab(v.city) || ''
        r[v.id]['makesLocal'] =
          (_.map v.makes, (m) -> CarMakers.getLab(m)).join(', ')
        v.phones ||= null
        v.addrs  ||= null
        v.emails ||= null
        r[v.id]['phones'] = _.map JSON.parse(v.phones), (p) ->
          p.label = PhoneTypes.getLab(p.key)
          p.note  ||= ''
          p
        r[v.id]['addrs'] = _.map JSON.parse(v.addrs), (p) ->
          p.label = AddressTypes.getLab(p.key)
          p
        r[v.id]['emails'] = _.map JSON.parse(v.emails), (p) ->
          p.label = EmailTypes.getLab(p.key)
          p
        phones = _.filter r[v.id]['phones'], ({key}) ->
          key == (if v.isdealer then 'serv' else 'disp')
        showPhone = phones?[0] or r[v.id]['phones']?[0]
        r[v.id]['phone']       = showPhone?.value || ''
        r[v.id]['workingTime'] = showPhone?.note  || ''
        r[v.id]['factAddr'] =
          (_.filter r[v.id]['addrs'], ({key}) -> key == 'fact')[0]?.value || ''
      r

    kvm["searchK"] = ko.computed(->kvm["search"]()).extend { throttle: 300 }
    kvm['searchProcessed'] = ko.computed ->
      sort = kvm['choosenSort']()[0]
      srvs = kvm['servicesLocals']()
      flt  = kvm['searchK']()
      s = for k, v of kvm['searchH']()
        v.services = _.sortBy v.services, (v) -> [v.priority2, v.priority3]
        v
      unless _.isEmpty srvs
        s = _.sortBy s, (v) ->
          parseInt (_.find v.services, (s) -> s.name == srvs[0].value)?[sort]
      if flt
        s = _.filter s, (v) -> checkMatch flt, v
      if kvm['workNow']()
        s = _.filter s, (v) ->
          k = if v.isdealer then 'serv' else 'disp'
          times = _.reduce (_.filter v.phones, ({key}) -> key == k),
                           ((a, {note}) -> "#{a}, #{(note or '')}"),
                           ''
          time.isWorkingNow time.parseWorkTimes(times)
      return s

    kvm['selectedPartner'] = ko.observable(null)

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
    city          : city
    make          : make
    dealer        : dlr
    mobilePartner : mbp
    workNow       : wn
    services      : srvs
    priority2     : pr2
    priority3     : pr3
