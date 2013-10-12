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

  # Build a place (structure with `coords` and `bounds` fields,
  # containind OpenLayers LonLat and Bounds, respectively) from
  # geoQuery response.
  #
  # Places use WSG projection for coordinates and bounds.
  buildPlace = (res) ->
    if res.error
      null
    else
      if res.length > 0
        # If possible, pick first result with osm_type = "relation",
        # because "node" results usually have no suitable bondingbox
        # property.
        el = _.find res, (r) -> r.osm_type == "relation"
        if not el?
          el = res[0]
        bb = el.boundingbox
        coords: new OpenLayers.LonLat el.lon, el.lat
        bounds: new OpenLayers.Bounds bb[2], bb[0], bb[3], bb[1]

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

  # Build a place for city from geoQuery response (overrides
  # coordinates and boundaries for certain key cities)
  buildCityPlace = (res) ->
    # TODO Remove this hack (#837) after Cities dict is implemented
    # properly
    switch res[0]?.osm_id
      when "102269"
        Moscow
      when "337422"
        Petersburg
      else
        buildPlace res

  # Read "32.54,56.21" (the way coordinates are stored in model
  # fields) into LonLat object (WSG projection)
  lonlatFromShortString = (coords) ->
    if coords?.length > 0
      parts = coords.split ","
      new OpenLayers.LonLat parts[0], parts[1]
    else
      null

  # Convert LonLat object (in WSG projection) to a string in format
  # "32.41,52.33"
  shortStringFromLonlat = (coords) ->
    if coords?
      return "#{coords.lon},#{coords.lat}"
    else
      null

  carIcon = "/s/img/car-icon.png"
  towIcon = "/s/img/tow-icon.png"
  busyTowIcon = "/s/img/busy-tow-icon.png"
  staleTowIcon = "/s/img/stale-tow-icon.png"
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

  # Center map on place bounds or coordinates
  setPlace = (osmap, place) -> fitPlaces osmap, [place]

  # Center an OSM on a city. City is a value from DealerCities
  # dictionary.
  centerMapOnCity = (osmap, city) ->
    if city?.length > 0
      fixed_city = global.dictValueCache.DealerCities[city]
      $.getJSON geoQuery(fixed_city), (res) ->
        if res.length > 0
          setPlace osmap, buildCityPlace res

  # Reposition and rezoom a map so that all places (see `buildPlace`)
  # fit. Set default place (Moscow) if places array is empty.
  fitPlaces = (osmap, places) ->
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
          place.bounds.clone().transform(wsgProj, osmProj), true)
      else
        osmap.zoomTo defaultZoomLevel
      # Then recenter on the very place for better positioning (your
      # experience may vary)
      osmap.setCenter place.coords.clone().transform wsgProj, osmProj

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
          # results
          if places.length == 2
            closefit = true
          bounds.extend p.coords

      gbounds = bounds.transform wsgProj, osmProj
      osmap.zoomToExtent gbounds, closefit

      # Occasionally closefitting occludes boundless places, so we fix
      # this
      ex = osmap.getExtent().transform osmProj, wsgProj
      for p in places
        if not ex.containsLonLat p.coords
          osmap.zoomToExtent gbounds, false
          break

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
    # Create new map only if does not exist yet
    if $(el).hasClass("olMap")
      osmap = $(el).data "osmap"
      only_reposition = true
    else
      osmap = new OpenLayers.Map(el.id)
      osmap.addLayer(new OpenLayers.Layer.OSM())

    fieldName = $(el).attr("name")
    view = $(mu.elementView($(el)))
    modelName = mu.elementModel($(el))
    kvm = u.findVM parentView

    coord_field = mu.modelField(modelName, fieldName).meta["targetCoords"]
    addr_field = mu.modelField(modelName, fieldName).meta["targetAddr"]
    city_field = mu.modelField(modelName, fieldName).meta["cityField"]
    current_blip_type =
      mu.modelField(modelName, fieldName).meta["currentBlipType"] or "default"

    # Place a blip and initialize places if coordinates are already
    # known
    places = []
    clearBlip osmap
    if coord_field?
      coords_string = kvm[coord_field]()
      if coords_string?.length > 0
        coords = lonlatFromShortString coords_string
        places = [coords: coords]
        currentBlip osmap, (coords.clone().transform wsgProj, osmProj),
          current_blip_type
    fitPlaces osmap, places

    # Show whole city on map if a valid city is set and coords have
    # not been recognized (fitting whole city when coords are set
    # makes no sense)
    if city_field?
      city = kvm[city_field]()
      if city?.length > 0
        fixed_city = global.dictValueCache.DealerCities[city]
        $.getJSON geoQuery(fixed_city), (res) ->
          if res.length > 0
            if _.isEmpty places
              fitPlaces osmap, [buildCityPlace res]

    # If the map already exists, stop here
    return if only_reposition

    # Setup handlers to update target address and coordinates if the
    # map is clickable
    if addr_field?
      osmap.events.register("click", osmap, (e) ->
        coords = osmap.getLonLatFromViewPortPx(e.xy)
                 .transform(osmProj, wsgProj)
        spliceCoords coords, (u.findVM parentView),
          coord_field: coord_field
          osmap: osmap
          current_blip_type: current_blip_type
          city_field: city_field
          addr_field: addr_field
      )

    $(el).data("osmap", osmap)

  # Move the current position blip on a map.
  #
  # - coords: OpenLayers.Coords, in OSM projection
  #
  # - type: one of types in iconFromType
  currentBlip = (osmap, coords, type) ->
    ico = new OpenLayers.Icon(iconFromType[type], iconSize)
    markers = reinstallMarkers(osmap, "CURRENT")
    markers.addMarker(
      new OpenLayers.Marker(coords, ico))

  # Clear current blip marker
  clearBlip = (osmap) ->
    reinstallMarkers osmap, "CURRENT"

  # Given a readable addres, try to fill a set of model fields,
  # updating coordinates and map position.
  #
  # Options is an object with following keys:
  #
  # - coord_field
  # - osmap
  # - current_blip_type
  #
  # Every key is optional.
  spliceAddress = (addr, kvm, options) ->
    $.getJSON(geoQuery(addr), (res) ->
      if res.length > 0
        lonlat = new OpenLayers.LonLat res[0].lon, res[0].lat

        if options.coord_field?
          kvm[options.coord_field] shortStringFromLonlat lonlat

        if options.osmap?
          setPlace options.osmap, buildPlace res
          currentBlip options.osmap,
            lonlat.clone().transform(wsgProj, osmProj),
            options.current_blip_type
    )

  # Give OpenLayers.Coords object (in WSG projection), try to fill a
  # set of model fields, updating text coordinates, map position, city
  # and address.
  #
  # Options is an object with following keys:
  #
  # - coord_field
  # - osmap
  # - current_blip_type
  # - addr_field
  # - city_field
  spliceCoords = (coords, kvm, options) ->
    osmCoords = coords.clone().transform wsgProj, osmProj

    if options.coord_field?
      kvm[options.coord_field] shortStringFromLonlat coords

    if options.osmap?
      currentBlip options.osmap, osmCoords, options.current_blip_type

    if options.addr_field? || options.city_field?
      $.getJSON(geoRevQuery(coords.lon, coords.lat),
      (res) ->
        addr = buildReverseAddress res

        if options.addr_field?
          kvm[options.addr_field](addr)

        if options.city_field?
          city = buildReverseCity(res)
          # Do not overwrite current city if new city is not
          # recognized
          if city?
            kvm[options.city_field](city)
      )

  # Forward geocoding picker (address -> coordinates)
  #
  # For field with this picker type, following metas are recognized:
  #
  # - targetMap: name of map field to write geocoding results into
  #              (recenter & set new blip on map)
  #
  # - currentBlipType: used for map blip when targetMap is set
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

    osmap = view.find("[name=#{map_field}]").data("osmap")

    kvm = u.findVM(viewName)

    spliceAddress addr, kvm,
      coord_field: coord_field
      osmap: osmap
      current_blip_type: current_blip_type

  # Reverse geocoding picker (coordinates -> address)
  #
  # Recognized field metas:
  #
  # - targetMap
  #
  # - targetAddr
  #
  # - cityField
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

    map_field = mu.modelField(modelName, fieldName).meta['targetMap']
    osmap = view.find("[name=#{map_field}]").data("osmap")

    setPlace osmap, coords: coords

    addr_field = mu.modelField(modelName, fieldName).meta['targetAddr']
    city_field = mu.modelField(modelName, fieldName).meta['cityField']
    current_blip_type =
      mu.modelField(modelName, map_field).meta["currentBlipType"] or "default"
    kvm = u.findVM(viewName)
    
    spliceCoords coords, kvm,
      osmap: osmap
      current_blip_type: current_blip_type
      addr_field: addr_field
      city_field: city_field

  # Coordinates picker for partner screen which uses a modal window to
  # render the map in. Recognizes the same field metas as initOSM.
  mapPicker = (field_name, el) ->
    modal = $("#partnerMapModal")
    map_el = modal.find(".osMap")[0]
    view_name = mu.elementView($(el)).id
    model_name = mu.elementModel($(el))
    kvm = u.findVM view_name

    search = modal.find("#map-search-field")
    search_button = modal.find("#map-search-button")

    city_field = mu.modelField(model_name, field_name).meta["cityField"]

    # Initialize search field with city if factAddr is empty
    if city_field? && _.isEmpty kvm['factAddr']()
      city = kvm[city_field]()
      if city?.length > 0
        fixed_city = global.dictValueCache.DealerCities[city]
        search.val(fixed_city)

    coord_field = mu.modelField(model_name, field_name).meta['targetCoords']
    current_blip_type =
      mu.modelField(model_name, field_name).meta["currentBlipType"] or "default"

    $("#partnerMapModal").one "shown", ->
      # Resize map container to fit the modal window container
      w = $(window).height()
      modal.find(".modal-body").css('max-height', w * 0.80)
      modal.find(".osMap").height(w * 0.5)

      # Activate simple map search
      search.keypress (e) ->
        if e.which == 13
          search_button.trigger "click"
      search_button.click () ->
        osmap = $(map_el).data("osmap")
        spliceAddress search.val(), kvm,
          coord_field: coord_field
          osmap: osmap
          current_blip_type: current_blip_type

      initOSM map_el, view_name

    # Unbind handlers to avoid multiple handler calls when the map
    # popup is shown again
    $("#partnerMapModal").one "hidden", ->
      search.off "keypress"
      search_button.off "click"

    $("#partnerMapModal").modal('show')

  { iconSize              : iconSize
  , defaultZoomLevel      : defaultZoomLevel
  , geoQuery              : geoQuery
  , buildPlace            : buildPlace
  , buildCityPlace        : buildCityPlace
  , Moscow                : Moscow
  , Petersburg            : Petersburg
  , wsgProj               : wsgProj
  , osmProj               : osmProj

  , lonlatFromShortString : lonlatFromShortString
  , shortStringFromLonlat : shortStringFromLonlat

  , carIcon               : carIcon
  , towIcon               : towIcon
  , busyTowIcon           : busyTowIcon
  , staleTowIcon          : staleTowIcon
  , partnerIcon           : partnerIcon
  , dealerIcon            : dealerIcon
  , hlIconName            : hlIconName
  , reinstallMarkers      : reinstallMarkers
  , fitPlaces             : fitPlaces

  , initOSM               : initOSM
  , currentBlip           : currentBlip

  , spliceCoords          : spliceCoords
  , spliceAddress         : spliceAddress

  , geoPicker             : geoPicker
  , reverseGeoPicker      : reverseGeoPicker
  , mapPicker             : mapPicker
  }
