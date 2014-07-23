define [], ->
  # This module maps server <-> client data types
  # c2s prefix means client  -> server
  # s2c prefix means client <-  server

  c2sDate = (fmt) -> (v) ->
    date = Date.parseExact(v, fmt)
    if date
      String(Math.round(date.getTime() / 1000))
    else
      # FIXME: really what should this do with wrong dates?
      console.error("datamap: can't parse date '#{v}' with '#{fmt}'")
      ""

  # How a Day is formatted on client
  guiDayFormat = "dd.MM.yyyy"

  # How a UTCTime is formatted on client (must match mask for datetime
  # field templates)
  guiUTCTimeFormat = "dd.MM.yyyy HH:mm:ss"

  # Server format for Day (ISO 8601)
  serverDayFormat = "yyyy-MM-dd"

  # Parse a formatted string to ISO 8601 Date
  parseISO = (fmt) -> (v) ->
    date = Date.parseExact v, fmt
    if date
      date
    else
      if v != ""
        console.error("datamap: could not parse date '#{v}' with '#{fmt}'")
        null
      else
        ""
  s2cDate = (fmt) -> (v) ->
    return null if _.isEmpty v
    d = undefined
    d = new Date(v * 1000)
    return d.toString(fmt) if isFinite d
    d = Date.parseExact(v, "yyyy-MM-dd HH:mm:ssz")
    return d.toString(fmt) if not _.isNull(d) && isFinite d

  # Convert ISO 8601 date/time object to a formatted string
  s2cISO = (fmt) -> (v) ->
    return null if _.isEmpty v
    new Date(v).toString fmt

  s2cJson = (v) ->
    return null if _.isEmpty v
    JSON.parse v

  c2sDay = (v) -> ((parseISO guiDayFormat) v)?.toString serverDayFormat

  c2sDictSetInt = (vals) -> nullOnEmpty _.map vals, (v) -> parseInt v

  c2sDictSetText = (vals) ->
    # Force stringifying of ints, anyway we can't save int[] in such field
    nullOnEmpty _.map vals, (v) -> if _.isNumber v then String v else v

  nullOnEmpty = (v) ->
    # Convert empty arrays to null (otherwise the server gets confused
    # about types)
    if _.isEmpty v then null else v

  c2sTypes =
    'dictionary-set-int':  c2sDictSetInt
    'dictionary-set-text': c2sDictSetText
    'dictionary-many': (v) -> (v?.join ',') || ''
    checkbox  : (v) -> if v then "1" else "0"
    Bool      : (v) -> v
    Integer   : (v) -> parseInt v
    Double    : (v) -> parseFloat v.replace ',', '.'
    Day       : c2sDay
    UTCTime   : (v) -> ((parseISO guiUTCTimeFormat) v)?.toISOString()
    IdentList : (v) -> v
    dictionary: (v) -> if _.isNull v then '' else v
    date      : c2sDate("dd.MM.yyyy")
    datetime  : c2sDate("dd.MM.yyyy HH:mm:ss")
    JsonAsText: JSON.stringify
    JSON      : (v) -> v
    ident     : (v) -> parseInt v
    'interval-date' : (v) -> v.map c2sDay
    'interval-datetime': (v) -> v.map (t) ->
      Date.parseExact(t, "dd.MM.yyyy")?.toString "yyyy-MM-ddTHH:mm:ss.0Z"

  s2cTypes =
    'dictionary-set-int':  _.identity
    'dictionary-set-text': _.identity
    'dictionary-many': (v) -> if _.isEmpty v then [] else v.split(',')
    checkbox  : (v) -> v == "1"
    Bool      : (v) -> v
    Integer   : (v) -> v
    Double    : (v) -> v
    IdentList : (v) -> v
    Day       : s2cISO guiDayFormat
    UTCTime   : s2cISO guiUTCTimeFormat
    dictionary: (v) -> v
    date      : s2cDate("dd.MM.yyyy")
    datetime  : s2cDate("dd.MM.yyyy HH:mm:ss")
    JsonAsText: s2cJson
    JSON      : (v) -> v

  defaultc2s = (v) -> if _.isNull(v) then "" else String(v)
  c2s = (val, type) -> (c2sTypes[type] || defaultc2s)(val)
  s2c = (val, type) -> (s2cTypes[type] || _.identity)(val)

  mapObj = (mapper) -> (obj, types) ->
    r = {}
    r[k] = mapper(v, types[k]) for k, v of obj
    r

  modelTypes = (model) ->
    _.foldl model.fields, ((m, f) -> m[f.name] = f.type; m), {}

  class Mapper
    constructor: (model) ->
      @types = modelTypes(model)

    c2sObj: (obj) => mapObj(c2s)(obj, @types)
    s2cObj: (obj) => mapObj(s2c)(obj, @types)

  c2s    : c2s
  s2c    : s2c
  c2sObj : mapObj(c2s)
  s2cObj : mapObj(s2c)
  Mapper : Mapper
  guiDayFormat    : guiDayFormat
  guiUTCTimeFormat: guiUTCTimeFormat
  serverDayFormat : serverDayFormat
