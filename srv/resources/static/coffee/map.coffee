# Build readable address from reverse Nominatim JSON response
this.buildReverseAddress = (res) ->
  addr = (res.address.road || res.address.pedestrian)

  if (_.isUndefined(res.address.house_number))
    return addr
  else
    return addr +  ", " + res.address.house_number


# Default marker icon size
this.iconSize = new OpenLayers.Size(40, 40)


# Erase existing marker layer and install a new one of the same name
this.reinstallMarkers = (osmap, layerName) ->
  layers = osmap.getLayersByName(layerName)
  if (!_.isEmpty(layers))
    osmap.removeLayer(layers[0])
  new_layer = new OpenLayers.Layer.Markers(layerName)
  osmap.addLayer(new_layer)

  return new_layer

# Setup OpenLayers map
this.initOSM = (el) ->
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
    16)
  nominatimRevQuery =
      "http://nominatim.openstreetmap.org/reverse.php?format=json&accept-language=ru-RU,ru&"

  markers = new OpenLayers.Layer.Markers("Car")
  osmap.addLayer(markers)

  partners = new OpenLayers.Layer.Markers("Partners")
  osmap.addLayer(partners)

  # Clickable maps may write new address to "target-addr" field and city to "target-city"
  if ($(el).data("target-addr"))
    osmap.events.register("click", osmap, (e) ->
      coords = osmap.getLonLatFromViewPortPx(e.xy)
               .transform(new OpenLayers.Projection("EPSG:900913"),
                          new OpenLayers.Projection("EPSG:4326"))
      $.getJSON(nominatimRevQuery + "lon=#{coords.lon}&lat=#{coords.lat}", (res) ->
        $.getJSON("/geo/partners/#{coords.lon},#{coords.lat}/0.5", (pres) ->
          addr = buildReverseAddress(res)

          addr_field = $(el).data("target-addr")
          city_field = $(el).data("target-city")
          global.viewsWare['case-form'].knockVM[addr_field](addr)
          # global.viewsWare['case-form'].knockVM[city_field](res.address.city)

          carBlip(osmap, osmap.getLonLatFromViewPortPx(e.xy))
          partnerBlips(osmap, pres)))
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
                   .transform(new OpenLayers.Projection("EPSG:4326"),
                              new OpenLayers.Projection("EPSG:900913"))
      markers.addMarker(
        new OpenLayers.Marker(
          coords, new OpenLayers.Icon("/s/img/tow-icon.png", iconSize)))


this.geoPicker = (fieldName, el) ->
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
              new OpenLayers.Projection("EPSG:900913"))
              , 16)
      carBlip(osmap.data("osmap"), osmap.data("osmap").getCenter()))
