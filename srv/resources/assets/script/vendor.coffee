# polyfills for IE11
require "mdn-polyfills/Object.assign"
window.Promise = require "bluebird"
require "whatwg-fetch"

$ = require "oldLegacy3p/jquery-2.2.4"
# Not using it right now:
# compatibility with jquery 2.x
# $ = require "oldLegacy3p/myJQuery"
# $.migrateMute = true if process.env.NODE_ENV is "production"

require "jquery.knob"
require "jquery.notify"
require "jquery.datatables"
require "jquery.typeahead"

md5      = require "blueimp-md5"
{Base64} = require "js-base64"

_         = require "underscore"
Immutable = require "immutable"
Mustache  = require "mustache"
markdown  = require("markdown").markdown
ko        = require "knockout"
{Finch}   = require "finch"

Redux          = require "redux"
ReduxThunk     = require("redux-thunk").default
ReduxActions   = require "redux-actions"
ReduxImmutable = require "redux-immutable"

moment = require "moment"
require "moment/locale/ru"
require "moment.tz"
moment.locale "ru"

require "bootstrap"
require "bootstrap-datepicker"
require "bootstrap-datepicker.ru"
require "bootstrap-daterangepicker"
require "bootstrap.jasny"

wysihtml5 = require "bootstrap.wysihtml5"
window.wysihtml5 = Object.assign wysihtml5, window.wysihtml5
require "bootstrap.wysihtml5.ru-RU"

Spinner = require "spin"
require "spin/jquery.spin"

d3         = require "oldLegacy3p/d3-v3.5.17"
Mousetrap  = require "mousetrap"

OpenLayers = require "ol2"
OpenLayers.Util.getImageLocation = (img) ->
  switch img
    when "blank.gif"
      require "ol2/img/blank.gif"
    when "cloud-popup-relative.png"
      require "ol2/img/cloud-popup-relative.png"
    when "drag-rectangle-off.png"
      require "ol2/img/drag-rectangle-off.png"
    when "drag-rectangle-on.png"
      require "ol2/img/drag-rectangle-on.png"
    when "east-mini.png"
      require "ol2/img/east-mini.png"
    when "layer-switcher-maximize.png"
      require "ol2/img/layer-switcher-maximize.png"
    when "layer-switcher-minimize.png"
      require "ol2/img/layer-switcher-minimize.png"
    when "marker-blue.png"
      require "ol2/img/marker-blue.png"
    when "marker-gold.png"
      require "ol2/img/marker-gold.png"
    when "marker-green.png"
      require "ol2/img/marker-green.png"
    when "marker.png"
      require "ol2/img/marker.png"
    when "measuring-stick-off.png"
      require "ol2/img/measuring-stick-off.png"
    when "measuring-stick-on.png"
      require "ol2/img/measuring-stick-on.png"
    when "north-mini.png"
      require "ol2/img/north-mini.png"
    when "panning-hand-off.png"
      require "ol2/img/panning-hand-off.png"
    when "panning-hand-on.png"
      require "ol2/img/panning-hand-on.png"
    when "slider.png"
      require "ol2/img/slider.png"
    when "south-mini.png"
      require "ol2/img/south-mini.png"
    when "west-mini.png"
      require "ol2/img/west-mini.png"
    when "zoom-minus-mini.png"
      require "ol2/img/zoom-minus-mini.png"
    when "zoom-plus-mini.png"
      require "ol2/img/zoom-plus-mini.png"
    when "zoom-world-mini.png"
      require "ol2/img/zoom-world-mini.png"
    when "zoombar.png"
      require "ol2/img/zoombar.png"
    else
      throw new Error "Unexpected requested OpenLayers image: '#{img}'"

# TODO maybe replace it with the moment library?
require "oldLegacy3p/date/core"
require "oldLegacy3p/date/ru-RU.js"
require "oldLegacy3p/date/extras.js"
require "oldLegacy3p/date/parser.js"
require "oldLegacy3p/date/sugarpak.js"

module.exports = {
  $
  _
  ko
  d3
  md5
  moment
  markdown

  Redux
  ReduxThunk
  ReduxActions
  ReduxImmutable

  Finch
  Base64
  Spinner
  Mustache
  Immutable
  Mousetrap
  OpenLayers
}
