# TODO Use city in reverse geocoding routines!

define ["model/utils", "utils"], (mu, u) ->

  # Default marker icon size
  iconSize = new OpenLayers.Size(40, 40)

  # Default map zoom level
  zoomLevel = 14

  # Low zoom level (partner map overview)
  beyondTheClouds = 10

  # Trailing slash included
  nominatimHost = "http://nominatim.openstreetmap.org/"

  geoRevQuery = (lon, lat) -> "/geo/revSearch/#{lon},#{lat}/"

  geoQuery = (addr) ->
    return nominatimHost +
      "search?format=json&accept-language=ru-RU,ru&q=#{addr}"

  wsgProj = new OpenLayers.Projection("EPSG:4326")
  osmProj = new OpenLayers.Projection("EPSG:900913")

  # Center on Moscow by default
  defaultCoords = new OpenLayers.LonLat(37.617874,55.757549)

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

    coord_field = mu.modelField(modelName, fieldName).meta["targetCoords"]
    addr_field = mu.modelField(modelName, fieldName).meta["targetAddr"]
    addrs_field = mu.modelField(modelName, fieldName).meta["targetAddrs"]
    city_field = mu.modelField(modelName, fieldName).meta["cityField"]
    current_blip_type =
      mu.modelField(modelName, fieldName).meta["currentBlipType"] or "default"

    if city_field?
      city_meta = u.splitFieldInView city_field, parentView

    # Center on the default location first
    osmap.setCenter(defaultCoords.clone().transform(wsgProj, osmProj),
                   zoomLevel)

    # Place a blip and recenter if coordinates are already known
    if coord_field?
      coord_meta = u.splitFieldInView(coord_field, parentView)

      coords = u.findVM(coord_meta.view)[coord_meta.field]()
      if coords? && coords.length > 0
        coords = lonlatFromShortString coords
        osmap.setCenter coords.transform(wsgProj, osmProj), zoomLevel
        currentBlip osmap, coords, current_blip_type
      else
        # Otherwise, just center on the city, not placing a blip
        if city_field?
          city = u.findVM(city_meta.view)[city_meta.field]()
          centerMapOnCity osmap, city

    # Setup handler to update target address and coordinates if the
    # map is clickable
    if addr_field? or addrs_field?
      if addr_field?
        addr_meta = u.splitFieldInView(addr_field, parentView)
      if addrs_field?
        addrs_meta = u.splitFieldInView(addrs_field, parentView)

      osmap.events.register("click", osmap, (e) ->
        coords = osmap.getLonLatFromViewPortPx(e.xy)
                 .transform(osmProj, wsgProj)

        if coord_field?
          # coord_field and coord_meta are already known as per
          # coord_field? branch in geocoding setup
          u.findVM(coord_meta.view)[coord_meta.field](shortStringFromLonlat coords)

        $.getJSON(geoRevQuery(coords.lon, coords.lat),
        (res) ->
          addr = buildReverseAddress(res)
          if addr_meta?
            u.findVM(addr_meta.view)[addr_meta.field](addr)

          # Write address to first "fact" address of partner
          if addrs_meta?
            json = u.findVM(addrs_meta.view)[addrs_meta.field]()
            u.findVM(addrs_meta.view)[addrs_meta.field](
              setKeyedJsonValue json, "fact", addr)

          if city_field?
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


  # Center an OSM on a city. City is a value from DealerCities
  # dictionary.
  centerMapOnCity = (osmap, city) ->
    if city? && city.length > 0
      fixed_city = global.dictValueCache.DealerCities[city]
      $.getJSON geoQuery(fixed_city), (res) ->
        if res.length > 0
          lonlat = new OpenLayers.LonLat res[0].lon, res[0].lat
          osmap.setCenter lonlat.transform(wsgProj, osmProj), zoomLevel


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
  , zoomLevel             : zoomLevel
  , beyondTheClouds       : beyondTheClouds
  , nominatimHost         : nominatimHost
  , geoRevQuery           : geoRevQuery
  , geoQuery              : geoQuery
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
  , lonlatFromShortString : lonlatFromShortString
  , shortStringFromLonlat : shortStringFromLonlat
  , geoPicker             : geoPicker
  , reverseGeoPicker      : reverseGeoPicker
  , mapPicker             : mapPicker
  }
