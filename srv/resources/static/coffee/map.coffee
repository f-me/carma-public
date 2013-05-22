# TODO Use city in reverse geocoding routines!

define ["model/utils", "utils"], (mu, u) ->

  # Default marker icon size
  iconSize = new OpenLayers.Size(50, 50)

  # Default map zoom level
  zoomLevel = 16

  # Low zoom level (partner map overview)
  beyondTheClouds = 10

  # Trailing slash included
  nominatimHost = "http://nominatim.openstreetmap.org/"

  geoRevQuery = (lon, lat) -> "/geo/revSearch/#{lon},#{lat}/"

  nominatimQuery = (addr) ->
    fixed_addr = addr.replace(/Москва/g, "Московская область")
    return nominatimHost +
      "search?format=json&accept-language=ru-RU,ru&q=#{fixed_addr}"

  wsgProj = new OpenLayers.Projection("EPSG:4326")
  osmProj = new OpenLayers.Projection("EPSG:900913")

  carIcon = "/s/img/car-icon.png"
  towIcon = "/s/img/tow-icon.png"
  partnerIcon = "/s/img/partner-icon.png"
  dealerIcon = "/s/img/dealer-icon.png"

  iconFromType =
    default: carIcon
    car: carIcon
    tow: towIcon
    partner: partnerIcon
    dealer: dealerIcon

  # Given regular icon name, return name of highlighted icon
  #
  # Filenames must follow the convention that original icons are named
  # as foo-icon.png and highlighted icons are named as foo-hl-icon.png.
  hlIconName = (filename) -> filename.replace("-icon", "-hl-icon")

  # Build readable address from reverse Nominatim JSON response
  buildReverseAddress = (res) ->
    if (res.error)
      null
    else
      res.address

  # Build city field value (or null, if the city is unknown)
  buildReverseCity = (res) ->
    if (res.error)
      null
    else
      global.dictLabelCache.DealerCities[res.city] || null

  # Erase existing marker layer and install a new one of the same name
  reinstallMarkers = (osmap, layerName) ->
    layers = osmap.getLayersByName(layerName)
    if (!_.isEmpty(layers))
      osmap.removeLayer(layers[0])
    new_layer = new OpenLayers.Layer.Markers(layerName)
    osmap.addLayer(new_layer)

    return new_layer

  # Setup OpenLayers map
  #
  # - parentView: parent view this map belongs to. This is used to set
  #               partner data in the parent VM when clicking partner
  #               blips.
  #
  # Supported meta annotations for a map field:
  #
  # - targetAddr: if set, map will be clickable, enabled for reverse
  #               geocoding and clicking the map will write address to
  #               this field of model. If the annotation has the form of
  #               `view_name/field_name`, then the field `field_name` in
  #               `view_name` view will be used instead of parent view.
  #
  # - targetCoords: read initial position & current blip from this field
  #                 of model; write geocoding results here (only if it's
  #                 enabled with `targetAddr` meta!). Metas of form
  #                 `case-form/field` are treated as in `targetAddr`.
  #
  # - cityField: contains city associated with the address shown on
  #              the map. Used to select initial map location when
  #              coordinates are not set, filled with city name when a
  #              new location is picked on the map.
  #
  # - moreCoords: this meta is a list of field names (possibly prefixed
  #               with view names as in `targetAddr`), where every field
  #               stores coordinates. Static blips for every field will
  #               be placed on the map. Blips are not updated as
  #               coordinates in the referenced fields change.
  #
  # - targetPartner: if set, map will show partner blips from table set
  #                  in `partnerTable` annotation on the same model. The
  #                  table must be present in the same view. Clicking a
  #                  blip will write partner information to fields of
  #                  the model as set in metas:
  #
  #                  partner name    → targetPartner
  #                  partner id      → targetPartnerId
  #                  partner address → targetPartnerAddr
  #                  partner coords  → targetPartnerCoords
  #
  # - highlightIdFields: a list of fields of the same model which
  #                      contain ids of partners which are to be
  #                      highlighted on the map
  #
  # - currentBlipType: one of types listed in the iconFromType map, used
  #                    to set the name icon used fo the «current blip».
  #                    Current blip is enabled only when geocoding is
  #                    active (see targetAddr).
  initOSM = (el, parentView) ->
    # Recenter map if it already exists to account for partner position
    # updates
    if $(el).hasClass("olMap")
      $(el).data("osmap").events.triggerEvent("moveend")
      return

    fieldName = $(el).attr("name")
    view = $(mu.elementView($(el)))
    modelName = mu.elementModel($(el))

    osmap = new OpenLayers.Map(el.id)
    osmap.addLayer(new OpenLayers.Layer.OSM())

    # Default location
    osmap.setCenter(new OpenLayers.LonLat(37.617874,55.757549)
                    .transform(wsgProj, osmProj),
                    zoomLevel)

    coord_field = mu.modelField(modelName, fieldName).meta["targetCoords"]
    addr_field = mu.modelField(modelName, fieldName).meta["targetAddr"]
    city_field = mu.modelField(modelName, fieldName).meta["cityField"]
    current_blip_type =
      mu.modelField(modelName, fieldName).meta["currentBlipType"] or "default"

    ## Bind the map to geocode address & coordinates

    # Place a blip and recenter if coordinates are already known
    if coord_field?
      coord_meta = u.splitFieldInView(coord_field, parentView)

      coords = u.findVM(coord_meta.view)[coord_meta.field]()
      if coords?
        coords = lonlatFromShortString coords
        osmap.setCenter coords.transform(wsgProj, osmProj), zoomLevel
        currentBlip osmap, coords, current_blip_type

    # Setup handler to update target address and coordinates if the
    # map is clickable
    if addr_field?
      addr_meta = u.splitFieldInView(addr_field, parentView)

      osmap.events.register("click", osmap, (e) ->
        coords = osmap.getLonLatFromViewPortPx(e.xy)
                 .transform(osmProj, wsgProj)

        if coord_field?
          # coord_view_name and coord_field are already known as per
          # coord_field? branch in geocoding setup
          u.findVM(coord_meta.view)[coord_meta.field](shortStringFromLonlat coords)

        $.getJSON(geoRevQuery(coords.lon, coords.lat),
        (res) ->
          addr = buildReverseAddress(res)
          u.findVM(addr_meta.view)[addr_meta.field](addr)

          if city_field?
            city_meta = u.splitFieldInView(city_field, parentView)
            city = buildReverseCity(res)
            u.findVM(city_meta.view)[city_meta.field](city)

          currentBlip osmap, osmap.getLonLatFromViewPortPx(e.xy), current_blip_type
        )
      )

    ## Read coordinates of static coordinate blips and place them on the map
    more_coord_field = mu.modelField(modelName, fieldName).meta["moreCoords"]
    if more_coord_field?
      more_coord_metas = _.map more_coord_field, u.splitFieldInView
      more_coords = _.map more_coord_metas, (fm) -> u.findVM(fm.view)[fm.field]()
      for c in more_coords
        if c?
          extraBlip osmap, (lonlatFromShortString c).transform(wsgProj, osmProj), "Extras"

    ## Bind map to partner list

    partner_field = mu.modelField(modelName, fieldName).meta["targetPartner"]

    if partner_field?
      partner_id_field =
        mu.modelField(modelName, fieldName).meta["targetPartnerId"]
      partner_addr_field =
        mu.modelField(modelName, fieldName).meta["targetPartnerAddr"]
      partner_coords_field =
        mu.modelField(modelName, fieldName).meta["targetPartnerCoords"]

      table_field = mu.modelField(modelName, fieldName).meta["partnerTable"]
      table = view.find("table##{table_field}")

      hl_fields = mu.modelField(modelName, fieldName).meta["highlightIdFields"]
      # Redraw partner blips on map when dragging or zooming
      osmap.events.register("moveend", osmap, (e) ->
        # Calculate new bounding box
        bounds = osmap.getExtent()
        pts = bounds.toArray()
        a = new OpenLayers.LonLat(pts[0], pts[1])
        b = new OpenLayers.LonLat(pts[2], pts[3])
        a.transform(osmProj, wsgProj)
        b.transform(osmProj, wsgProj)
        $.getJSON("/geo/partners/#{a.lon},#{a.lat}/#{b.lon},#{b.lat}/", (pres) ->
          # Use cache from table beneath a map for partner metadata
          partnerBlips(
            osmap, pres, table.data("cache"),
            parentView,
            # Fetch current values of fields listed in highlightIdFields
            _.map(hl_fields,
              (f) -> u.findVM(parentView)[f]()),
            partner_id_field, partner_field, partner_addr_field, partner_coords_field)
        )
      )
      # This is a workaround to make sure table cache is loaded prior to
      # partner markers rendering.
      _.delay(
        () ->
         osmap.setCenter(osmap.getCenter(), beyondTheClouds, false, true)
        500)

    $(el).data("osmap", osmap)


  # Move the current position blip on a map.
  #
  # - type: one of types in iconFromType
  currentBlip = (osmap, coords, type) ->
    ico = new OpenLayers.Icon(iconFromType[type], iconSize)
    markers = reinstallMarkers(osmap, "CURRENT")
    markers.addMarker(
      new OpenLayers.Marker(coords, ico))


  # Place the blip on a (possibly existing) layer of a map, preserving
  # the existing blips. Extra blips use the "default" icon.
  extraBlip = (osmap, coords, layerName) ->
    layers = osmap.getLayersByName(layerName)
    if (!_.isEmpty(layers))
      layer = layers[0]
    else
      layer = new OpenLayers.Layer.Markers(layerName)
      osmap.addLayer(layer)

    ico = new OpenLayers.Icon(iconFromType.default, iconSize)
    layer.addMarker(
      new OpenLayers.Marker(coords, ico))


  # Render list of partner markers on the map
  #
  # Arguments:
  #
  # - osmap: map to render on
  #
  # - partners: a list of [id, lon, lat, isDealer, isMobile] 5-tuples
  #
  # - tableCache: a hash of all partners, where key is id and value is
  #               an object with fields "name", "addrDeFacto", "phone1",
  #               "workingTime", "isMobile"
  #
  # - highlightIds: list of "partner:N" strings for highlighted partners
  #
  # - parentView: parentView for contractor
  #
  # - partnerField: clicking a button in marker popup will set this
  #                 value in given VM to partner name
  #
  # - partnerAddrField: same as partnerField, but for partner address
  # - partnerCoordsField: ... but for partner coordinates
  # - partnerIdField: ... but for partner id
  partnerBlips = (osmap,
                  partners, tableCache,
                  parentView,
                  highlightIds,
                  partnerIdField, partnerField,
                  partnerAddrField, partnerCoordsField) ->
    markers = do (osmap) -> reinstallMarkers(osmap, "Partners")
    tpl = $("#partner-popup-template").html()

    for blip in partners
      do (blip) ->
        id = blip[0]
        # cache ids are numeric, highlightIds are strings (being values
        # of knockVM)
        hl = _.include(highlightIds, "partner:" + id.toString())

        partner_cache = tableCache[id]
        is_dealer = blip[3]
        is_mobile = blip[4]

        # Skip partners which are not in table or highlighted
        return if not (partner_cache or hl)

        coords = new OpenLayers.LonLat(blip[1], blip[2])

        # Readable coords in WSG
        string_coords = shortStringFromLonlat coords
        # Coords to use for map blip
        coords = coords.transform(wsgProj, osmProj)

        if is_mobile
          ico = towIcon
        else
          if is_dealer
            ico = dealerIcon
          else
            ico = partnerIcon

        if (hl)
          ico = hlIconName(ico)

        mrk = new OpenLayers.Marker(
            coords, new OpenLayers.Icon(ico, iconSize))

        # Show partner info from table cache when clicking marker
        if (partner_cache)
          mrk.events.register("click", mrk, (e) ->

            # Let popup know where to put new partner data
            extra_ctx =
              numid: id
              mapId: osmap.div.id
              parentView: parentView
              partnerField: partnerField
              partnerIdField: partnerIdField
              partnerAddrField: partnerAddrField
              partnerCoordsField: partnerCoordsField
              coords: string_coords
            ctx =_.extend(partner_cache, extra_ctx)

            popup = new OpenLayers.Popup.FramedCloud(
              partner_cache.id, mrk.lonlat,
              new OpenLayers.Size(200, 200),
              Mustache.render(tpl, ctx),
              null, true)

            osmap.addPopup(popup))
        markers.addMarker(mrk)


  # Splice partner data into specified fields of a reference
  #
  # TODO We have to store all data in the associated HTML because
  # partner table is in a different view and thus is inaccessible.
  pickPartnerBlip = (
     referenceView, mapId,
     partnerId, partnerName, partnerAddr, partnerCoords,
     partnerIdField, partnerField, partnerAddrField, partnerCoordsField) ->

    $("#" + mapId).data("osmap").events.triggerEvent("moveend")
    vm = u.findVM(referenceView)
    vm[partnerIdField]("partner:" + partnerId)
    vm[partnerField](partnerName)
    vm[partnerAddrField](partnerAddr)
    vm[partnerCoordsField](partnerCoords)


  # Read "32.54,56.21" (the way coordinates are stored in model fields)
  # into LonLat object
  lonlatFromShortString = (coords) ->
    parts = coords.split(",")
    return new OpenLayers.LonLat(parts[0], parts[1])


  # Convert LonLat object to string in format "32.41,52.33"
  shortStringFromLonlat = (coords) ->
    return "#{coords.lon},#{coords.lat}"


  # Forward geocoding picker (address -> coordinates)
  #
  # For field with this picker type, following metas are recognized:
  #
  # - targetMap: name of map field to write geocoding results into
  #              (recenter & set new blip on map)
  #
  # - targetCoords: name of field to write geocoding results into
  #                 (coordinates in "lon, lat" format). This meta is
  #                 also used by the map to set the initial position
  #                 (see initOSM docs).
  #
  # Arguments are picker field name and picker element.
  geoPicker = (fieldName, el) ->
    addr = $(el).parents('.input-append')
                .children("input[name=#{fieldName}]")
                .val()

    viewName = mu.elementView($(el)).id
    view = $(mu.elementView($(el)))
    modelName = mu.elementModel $(el)

    coord_field = mu.modelField(modelName, fieldName).meta['targetCoords']
    map_field = mu.modelField(modelName, fieldName).meta['targetMap']
    current_blip_type =
      mu.modelField(modelName, map_field).meta["currentBlipType"] or "default"

    $.getJSON(nominatimQuery(addr), (res) ->
      if res.length > 0
        lonlat = new OpenLayers.LonLat(res[0].lon, res[0].lat)

        if coord_field?
          u.findVM(viewName)[coord_field](shortStringFromLonlat lonlat)

        if map_field?
          osmap = view.find("[name=#{map_field}]").data("osmap")
          osmap.setCenter(
                lonlat.transform(wsgProj, osmProj),
                zoomLevel)
          currentBlip osmap, osmap.getCenter(), current_blip_type
    )


  # Reverse geocoding picker (coordinates -> address)
  #
  # Recognized field metas:
  #
  # - targetMap
  #
  # - targetAddr
  #
  # - TODO cityField
  reverseGeoPicker = (fieldName, el) ->
    coords =
      lonlatFromShortString(
        $(el).parents('.input-append')
             .children("input[name=#{fieldName}]")
             .val())
    viewName = mu.elementView($(el)).id
    view = $(mu.elementView($(el)))
    modelName = mu.elementModel($(el))

    osmCoords = coords.clone().transform(wsgProj, osmProj)

    addr_field = mu.modelField(modelName, fieldName).meta['targetAddr']
    map_field = mu.modelField(modelName, fieldName).meta['targetMap']
    current_blip_type =
      mu.modelField(modelName, map_field).meta["currentBlipType"] or "default"

    if map_field?
      osmap = view.find("[name=#{map_field}]").data("osmap")
      osmap.setCenter(osmCoords, zoomLevel)
      currentBlip osmap, osmap.getCenter(), current_blip_type

    if addr_field?
      $.getJSON(geoRevQuery coords.lon, coords.lat,
        (res) ->
          addr = buildReverseAddress(res)
          u.findVM(viewName)[addr_field](addr)
      )

  # Coordinates picker which uses a modal window to render the map in.
  #
  # Fills a field with coordinates chosen on the map.
  #
  # Recognizes the same field metas as initOSM.
  mapPicker = (fieldName, el) ->
    coords =
      lonlatFromShortString(
        $(el).parents('.input-append')
             .children("input[name=#{fieldName}]")
             .val())

    viewName = mu.elementView($(el)).id
    view = $(mu.elementView($(el)))
    modelName = mu.elementModel($(el))

    osmCoords = coords.clone().transform(wsgProj, osmProj)

    addr_field = mu.modelField(modelName, fieldName).meta['targetAddr']

    mapEl = $("#partnerMapModal").find(".osMap")[0]
    $("#partnerMapModal").modal('show')

    if $(mapEl).hasClass("olMap")
      $(el).data("osmap").destroy()

    setTimeout((-> initOSM mapEl, viewName), 1000)

  { iconSize              : iconSize
  , zoomLevel             : zoomLevel
  , beyondTheClouds       : beyondTheClouds
  , nominatimHost         : nominatimHost
  , geoRevQuery           : geoRevQuery
  , nominatimQuery        : nominatimQuery
  , wsgProj               : wsgProj
  , osmProj               : osmProj
  , carIcon               : carIcon
  , towIcon               : towIcon
  , partnerIcon           : partnerIcon
  , dealerIcon            : dealerIcon
  , hlIconName            : hlIconName
  , buildReverseAddress   : buildReverseAddress
  , reinstallMarkers      : reinstallMarkers
  , initOSM               : initOSM
  , currentBlip           : currentBlip
  , extraBlip             : extraBlip
  , partnerBlips          : partnerBlips
  , pickPartnerBlip       : pickPartnerBlip
  , lonlatFromShortString : lonlatFromShortString
  , shortStringFromLonlat : shortStringFromLonlat
  , geoPicker             : geoPicker
  , reverseGeoPicker      : reverseGeoPicker
  , mapPicker             : mapPicker
  }
