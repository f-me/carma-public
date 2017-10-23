$ = require "jquery"
require "jquery.knob"
require "jquery.notify"
require "jquery.datatables"

md5      = require "blueimp-md5"
_        = require "underscore"
Mustache = require "mustache"
ko       = require "knockout"
Finch    = require "finchjs/coffee/finch"

moment = require "moment"
require "moment/locale/ru"
require "moment.tz"

require "bootstrap"
require "bootstrap-datepicker"
require "bootstrap-datepicker.ru"
require "bootstrap-daterangepicker"
require "bootstrap.jasny"
require "bootstrap.wysihtml5"
require "bootstrap.wysihtml5.ru-RU"

Spinner = require "spin"
require "spin/jquery.spin"

d3         = require "d3/d3"
Mousetrap  = require "mousetrap"
OpenLayers = require "ol2"

Promise = require "bluebird"
require "whatwg-fetch"

# old messy shit
# TODO FIXME kill me please someone
require "more-libs/date/core"
require "more-libs/date/ru-RU.js"
require "more-libs/date/extras.js"
require "more-libs/date/parser.js"
require "more-libs/date/sugarpak.js"

module.exports =
  { $
  , _
  , ko
  , d3
  , md5
  , moment
  , Finch
  , Mustache
  , Spinner
  , Mousetrap
  , OpenLayers
  , Promise
  }
