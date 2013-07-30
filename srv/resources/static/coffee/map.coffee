# TODO Use city in reverse geocoding routines!

define ["model/utils", "utils"], (mu, u) ->

  # Default marker icon size
  iconSize = new OpenLayers.Size(40, 40)

  # Default map zoom level (used when a POI displayed on map has no
  # bounds)
  defaultZoomLevel = 13
  
  geoRevQuery = (lon, lat) -> "/geo/revSearch/#{lon},#{lat}/"

  geoQuery = (addr) ->
    nominatimHost = "http://nominatim.openstreetmap.org/"
    return nominatimHost +
      "search?format=json&accept-language=ru-RU,ru&q=#{addr}"

  # Build readable address from reverse Nominatim JSON response
  buildReverseAddress = (res) ->
    if (res.error)
      null
    else
      if res.city?
        if res.address?
          res.city + ', ' + res.address
        else
          res.city
      else
        res.address

  # Build city field value (or null, if the city is unknown)
  buildReverseCity = (res) ->
    if (res.error)
      null
    else
      global.dictLabelCache.DealerCities[res.city] || null

  wsgProj = new OpenLayers.Projection("EPSG:4326")
  osmProj = new OpenLayers.Projection("EPSG:900913")

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

  # Read "32.54,56.21" (the way coordinates are stored in model fields)
  # into LonLat object
  lonlatFromShortString = (coords) ->
    if coords?.length > 0
      parts = coords.split(",")
      new OpenLayers.LonLat(parts[0], parts[1])
    else
      null

  # Convert LonLat object to string in format "32.41,52.33"
  shortStringFromLonlat = (coords) ->
    return "#{coords.lon},#{coords.lat}"

  carIcon = "/s/img/car-icon.png"
  towIcon = "/s/img/tow-icon.png"
  partnerIcon = "/s/img/partner-icon.png"
  dealerIcon = "/s/img/dealer-icon.png"

  # Map values "default", "car", "tow", "partner", "dealer" to icon
  # absolute paths
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
  #               this field of model.
  #
  # - targetAddrs
  #
  # - targetCoords: read initial position & current blip from this field
  #                 of model; write geocoding results here (only if it's
  #                 enabled with `targetAddr` meta!)
  #
  # - cityField: contains city associated with the address shown on
  #              the map. Used to select initial map location when
  #              coordinates are not set, filled with city name when a
  #              new location is picked on the map.
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
    kvm = u.findVM parentView

    osmap = new OpenLayers.Map(el.id)
    osmap.addLayer(new OpenLayers.Layer.OSM())

    coord_field = mu.modelField(modelName, fieldName).meta["targetCoords"]
    addr_field = mu.modelField(modelName, fieldName).meta["targetAddr"]
    addrs_field = mu.modelField(modelName, fieldName).meta["targetAddrs"]
    city_field = mu.modelField(modelName, fieldName).meta["cityField"]
    current_blip_type =
      mu.modelField(modelName, fieldName).meta["currentBlipType"] or "default"

    # Center on the default location first
    osmap.setCenter(Moscow.coords.clone().transform(wsgProj, osmProj),
                    defaultZoomLevel)

    # Place a blip and recenter if coordinates are already known
    if coord_field?
      coords = kvm[coord_field]()
      if coords? && coords.length > 0
        coords = lonlatFromShortString coords
        osmap.setCenter coords.transform(wsgProj, osmProj), defaultZoomLevel
        currentBlip osmap, coords, current_blip_type
      else
        # Otherwise, just center on the city, not placing a blip
        if city_field?
          city = kvm[city_field]()
          centerMapOnCity osmap, city

    # Setup handler to update target address and coordinates if the
    # map is clickable
    if addr_field? or addrs_field?
      osmap.events.register("click", osmap, (e) ->
        coords = osmap.getLonLatFromViewPortPx(e.xy)
                 .transform(osmProj, wsgProj)

        if coord_field?
          kvm[coord_field] shortStringFromLonlat coords

        $.getJSON(geoRevQuery(coords.lon, coords.lat),
        (res) ->
          addr = buildReverseAddress res
          
          if addr_field?
            kvm[addr_field](addr)

          # Write address to first "fact" address of partner
          if addrs_field?
            json = u.findVM(addrs_meta.view)[addrs_meta.field]()
            kvm[addrs_field] (u.setKeyedJsonValue json, "fact", addr)

          if city_field?
            city = buildReverseCity(res)
            kvm[city_field](city)

          currentBlip osmap, osmap.getLonLatFromViewPortPx(e.xy), current_blip_type
        )
      )

    $(el).data("osmap", osmap)

  # Move the current position blip on a map.
  #
  # - type: one of types in iconFromType
  currentBlip = (osmap, coords, type) ->
    ico = new OpenLayers.Icon(iconFromType[type], iconSize)
    markers = reinstallMarkers(osmap, "CURRENT")
    markers.addMarker(
      new OpenLayers.Marker(coords, ico))

  # Center an OSM on a city. City is a value from DealerCities
  # dictionary.
  centerMapOnCity = (osmap, city) ->
    if city? && city.length > 0
      fixed_city = global.dictValueCache.DealerCities[city]
      $.getJSON geoQuery(fixed_city), (res) ->
        if res.length > 0
          lonlat = new OpenLayers.LonLat res[0].lon, res[0].lat
          osmap.setCenter lonlat.transform(wsgProj, osmProj), defaultZoomLevel

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

    $.getJSON(geoQuery(addr), (res) ->
      if res.length > 0
        lonlat = new OpenLayers.LonLat(res[0].lon, res[0].lat)

        if coord_field?
          u.findVM(viewName)[coord_field](shortStringFromLonlat lonlat)

        if map_field?
          osmap = view.find("[name=#{map_field}]").data("osmap")
          osmap.setCenter(
                lonlat.transform(wsgProj, osmProj),
                defaultZoomLevel)
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

    if not coords?
      return

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

  # Coordinates picker for partner screen which uses a modal window to
  # render the map in.
  #
  # Fills a field with coordinates chosen on the map, writes spot
  # address to first "fact" address of `addrs` JSON field.
  #
  # Recognizes the same field metas as initOSM and `targetAddrs`.
  mapPicker = (fieldName, el) ->
    coords =
      lonlatFromShortString(
        $(el).parents('.input-append')
             .children("input[name=#{fieldName}]")
             .val())

    viewName = mu.elementView($(el)).id
    view = $(mu.elementView($(el)))
    modelName = mu.elementModel($(el))

    city_field = mu.modelField(modelName, fieldName).meta['cityField']
    blip_type = mu.modelField(modelName, fieldName).meta['currentBlipType']

    mapEl = $("#partnerMapModal").find(".osMap")[0]

    $("#partnerMapModal").one "shown", ->
      # Recenter the map if it already exists
      if $(mapEl).hasClass("olMap")
        oMap = $(mapEl).data("osmap")

        # Center on the default location first
        oMap.setCenter defaultCoords.clone().transform(wsgProj, osmProj)

        # Center on partner coordinates
        if coords?
          osmCoords = coords.clone().transform(wsgProj, osmProj)
          oMap.setCenter osmCoords
          currentBlip oMap, osmCoords, blip_type
        else
          # Otherwise, just center on the city, not placing a blip
          if city_field?
            city_meta = u.splitFieldInView city_field, viewName
            city = u.findVM(city_meta.view)[city_meta.field]()
            centerMapOnCity oMap, city

        oMap.events.triggerEvent "moveend"
      else
        initOSM mapEl, viewName

    $("#partnerMapModal").modal('show')

  { iconSize              : iconSize
  , defaultZoomLevel      : defaultZoomLevel
  , geoQuery              : geoQuery
  , Moscow                : Moscow
  , Petersburg            : Petersburg
  , wsgProj               : wsgProj
  , osmProj               : osmProj

  , lonlatFromShortString : lonlatFromShortString
  , shortStringFromLonlat : shortStringFromLonlat

  , carIcon               : carIcon
  , towIcon               : towIcon
  , partnerIcon           : partnerIcon
  , dealerIcon            : dealerIcon
  , hlIconName            : hlIconName
  , reinstallMarkers      : reinstallMarkers

  , initOSM               : initOSM
  , currentBlip           : currentBlip

  , geoPicker             : geoPicker
  , reverseGeoPicker      : reverseGeoPicker
  , mapPicker             : mapPicker
  }
