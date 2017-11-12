$ = require "jquery"
require "jquery.knob"
require "jquery.notify"
require "jquery.datatables"

md5      = require "blueimp-md5"
{Base64} = require "js-base64"

_        = require "underscore"
Mustache = require "mustache"
ko       = require "knockout"
{Finch}  = require "finchjs/coffee/finch"

moment = require "moment"
require "moment/locale/ru"
require "moment.tz"

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

d3         = require "d3/d3"
Mousetrap  = require "mousetrap"
OpenLayers = require "ol2"

Promise = require "bluebird"
require "whatwg-fetch"

# TODO maybe replace it with the moment library?
require "oldLegacy3p/date/core"
require "oldLegacy3p/date/ru-RU.js"
require "oldLegacy3p/date/extras.js"
require "oldLegacy3p/date/parser.js"
require "oldLegacy3p/date/sugarpak.js"

module.exports =
  { $
  , _
  , ko
  , d3
  , md5
  , Base64
  , moment
  , Finch
  , Mustache
  , Spinner
  , Mousetrap
  , OpenLayers
  , Promise
  }
