# TODO Use city in geocoding routines!

# Default marker icon size
this.iconSize = new OpenLayers.Size(50, 50)


# Default map zoom level
this.zoomLevel = 16


this.nominatimRevQuery =
  "http://nominatim.openstreetmap.org/reverse.php?format=json&accept-language=ru-RU,ru&"


this.nominatimQuery =
  "http://nominatim.openstreetmap.org/search?format=json&accept-language=ru-RU,ru&q="


this.wsgProj = new OpenLayers.Projection("EPSG:4326")

this.osmProj = new OpenLayers.Projection("EPSG:900913")


# Build readable address from reverse Nominatim JSON response
this.buildReverseAddress = (res) ->
  if (res.error)
    return null
    
  addr = (res.address.road || res.address.pedestrian)

  if (_.isUndefined(res.address.house_number))
    return addr
  else
    return addr +  ", " + res.address.house_number


# Erase existing marker layer and install a new one of the same name
this.reinstallMarkers = (osmap, layerName) ->
  layers = osmap.getLayersByName(layerName)
  if (!_.isEmpty(layers))
    osmap.removeLayer(layers[0])
  new_layer = new OpenLayers.Layer.Markers(layerName)
  osmap.addLayer(new_layer)

  return new_layer

# Setup OpenLayers map
#
# In case template for OL placeholder has appropriate
# data-target-addr, data-target-city and data-target-coords
# attributes, map will be enabled for clicking and reverse geocoding,
# writing appropriate geodata (address, city and coordinates) to the
# specified fields.
this.initOSM = (el) ->
  return if $(el).hasClass("olMap")

  fieldName = $(el).attr("name")

  osmap = new OpenLayers.Map(el.id)
  clicker = new OpenLayers.Handler.Click(el.id)

  osmap.addLayer(new OpenLayers.Layer.OSM())
  markers = new OpenLayers.Layer.Markers("Car")
  osmap.addLayer(markers)

  partners = new OpenLayers.Layer.Markers("Partners")
  osmap.addLayer(partners)

  # Default location
  osmap.setCenter(new OpenLayers.LonLat(37.617874,55.757549)
                  .transform(wsgProj, osmProj),
                  zoomLevel)


  # TODO Drop hardcoded name of the «real» parent view (case-form)
  coord_field = global.viewsWare["case-form"]
                .bbInstance.fieldHash[fieldName].meta['targetCoords']


  # Place a blip and recenter if coordinates are already known
  if coord_field?
    coords = lonlatFromShortString(global.viewsWare['case-form']
            .knockVM[coord_field]())
    if coords?
      osmap.setCenter(coords.transform(wsgProj, osmProj), zoomLevel)
      carBlip(osmap, coords)

  # Setup handler if map is clickable
  if ($(el).data("target-addr"))
    addr_field = global.viewsWare["case-form"]
                 .bbInstance.fieldHash[fieldName].meta['targetAddr']

    osmap.events.register("click", osmap, (e) ->
      coords = osmap.getLonLatFromViewPortPx(e.xy)
               .transform(osmProj, wsgProj)

      if coord_field?
        global.viewsWare['case-form']
        .knockVM[coord_field](coords.toShortString())

      $.getJSON(nominatimRevQuery + "lon=#{coords.lon}&lat=#{coords.lat}",
      (res) ->
        addr = buildReverseAddress(res)

        if addr_field?
          global.viewsWare['case-form'].knockVM[addr_field](addr)

        carBlip(osmap, osmap.getLonLatFromViewPortPx(e.xy))
      )
    )

  $(el).data("osmap", osmap)


# Move the car crash blip on the map
this.carBlip = (osmap, coords) ->
  ico = new OpenLayers.Icon("/s/img/car-icon.png", iconSize)
  markers = reinstallMarkers(osmap, "Car")
  markers.addMarker(
    new OpenLayers.Marker(coords, ico))


# Render list of partners on the map
#
# partners is a list of [id, lon, lat]
this.partnerBlips = (osmap, partners) ->
  markers = do (osmap) -> reinstallMarkers(osmap, "Partners")
  for blip in partners
    do (blip) ->
      coords = new OpenLayers.LonLat(blip[1], blip[2])
                   .transform(wsgProj, osmProj)
      markers.addMarker(
        new OpenLayers.Marker(
          coords, new OpenLayers.Icon("/s/img/tow-icon.png", iconSize)))


# Read "32.54, 56.21" (the way coordinates are stored in model fields)
# into LonLat object
this.lonlatFromShortString = (coords) ->
  parts = coords.split(", ")
  return new OpenLayers.LonLat(parts[0], parts[1])


# Forward geocoding picker
#
# geoPicker sets blip on a map and coordinates field placed in the
# same view as the picker field. map and coordinate field names are
# set with 'targetMap' and 'targetCoords' metas for picker field.
#
# Value of field specified in 'cityField' meta of picker is used with
# picker value to query Nominatim.
#
# Arguments are picker field name and picker element.
this.geoPicker = (fieldName, el) ->
  addr = $(el).parents('.input-append')
              .children("input[name=#{fieldName}]")
              .val()

  view = $(el).parents("[id*=view]")
  
  # TODO Drop hardcoded name of the «real» parent view (case-form)
  #
  # elementModel could be used, but global.models has no fieldHash
  # cache for easy field lookup
  coord_field = global.viewsWare["case-form"]
                .bbInstance.fieldHash[fieldName].meta['targetCoords']
                
  map_field = global.viewsWare["case-form"]
              .bbInstance.fieldHash[fieldName].meta['targetMap']

  city_field = global.viewsWare["case-form"]
               .bbInstance.fieldHash[fieldName].meta['cityField']

  if city_field?
    addr = global.viewsWare['case-form'].knockVM[city_field]() + ", " + addr

  $.getJSON(nominatimQuery+"#{addr}", (res) ->
    if res.length > 0
      lonlat = new OpenLayers.LonLat(res[0].lon, res[0].lat)

      if coord_field?
        global.viewsWare['case-form'].knockVM[coord_field](lonlat.toShortString())

      if map_field?
        osmap = view.find("[name=#{map_field}]").data("osmap")
        osmap.setCenter(
              lonlat.transform(wsgProj, osmProj),
              zoomLevel)
        carBlip(osmap, osmap.getCenter()))


# Reverse geocoding picker
#
# Performs reverse geocoding for address field and map in the same view.
# 
# Recognized field metas:
# 
# - targetAddr
# 
# - targetMap
this.reverseGeoPicker = (fieldName, el) ->
  coords =
    lonlatFromShortString(
      $(el).parents('.input-append')
           .children("input[name=#{fieldName}]")
           .val())
  view = $(el).parents("[id*=view]")

  osmCoords = coords.clone().transform(wsgProj, osmProj)

  # TODO Drop hardcoded name of the «real» parent view (case-form)
  map_field = global.viewsWare["case-form"]
              .bbInstance.fieldHash[fieldName].meta['targetMap']
  addr_field = global.viewsWare["case-form"]
                .bbInstance.fieldHash[fieldName].meta['targetAddr']

  if map_field?
    osmap = view.find("[name=#{map_field}]").data("osmap")
    osmap.setCenter(osmCoords, zoomLevel)
    carBlip(osmap, osmap.getCenter())

  if addr_field?
    $.getJSON(nominatimRevQuery + "lon=#{coords.lon}&lat=#{coords.lat}",
      (res) ->
        addr = buildReverseAddress(res)

        global.viewsWare['case-form'].knockVM[addr_field](addr)
        # global.viewsWare['case-form'].knockVM[city_field](res.address.city)
    )
