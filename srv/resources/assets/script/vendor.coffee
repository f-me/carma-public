require "normalize-styles"

$ = require "jquery"
require "jquery.knob"
require "jquery.notify"
require "jquery.datatables"

md5 = require "blueimp-md5"
_ = require "underscore"
mustache = require "mustache"
ko = require "knockout"

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

d3 = require "d3/d3"
Mousetrap = require "mousetrap"
OpenLayers = require "ol2"

# old messy shit
# TODO FIXME kill me please someone
require "more-libs/date/core"
require "more-libs/date/ru-RU.js"
require "more-libs/date/extras.js"
require "more-libs/date/parser.js"
require "more-libs/date/sugarpak.js"

module.exports =
  { $
  , md5
  , _
  , mustache
  , ko
  , moment
  , Spinner
  , d3
  , Mousetrap
  , OpenLayers
  }
