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

  # How a UTCTime is formatted on client
  guiUTCTimeFormat = "dd.MM.yyyy HH:mm"

  # Convert a formatted string to ISO 8601 string
  c2sISO = (fmt) -> (v) ->
    date = Date.parseExact v, fmt
    if date
      date.toISOString()
    else
      console.error("datamap: could not parse date '#{v}' with '#{fmt}'")
      null

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
    JSON.parse(v)

  c2sDictSet = (vals) ->
    ids = _.map vals, (v) -> parseInt v
    # check type of keys, we have in dict, it may be Text or Int
    # TODO: move uniq check to hooks when typed dictionaries appears
    res = if _.any ids, _.isNaN
            _.uniq vals
          else
            _.uniq ids
    # Convert empty arrays to null (otherwise the server gets confused
    # about types)
    if _.isEmpty res
      null
    else
      res

  c2sTypes =
    'dictionary-set': c2sDictSet
    'dictionary-many': (v) -> (c2sDictSet(v)?.join ',') || ''
    checkbox  : (v) -> if v then "1" else "0"
    Bool      : (v) -> v
    Integer   : (v) -> parseInt v
    Double    : (v) -> parseFloat v.replace ',', '.'
    Day       : (v) -> ((c2sISO guiDayFormat) v)?.slice(0, 10)
    UTCTime   : c2sISO guiUTCTimeFormat
    IdentList : (v) -> v
    dictionary: (v) -> if _.isNull v then '' else v
    date      : c2sDate("dd.MM.yyyy")
    datetime  : c2sDate("dd.MM.yyyy HH:mm")
    json      : JSON.stringify
    ident     : (v) -> parseInt v
    'interval-datetime': (v) ->
      v.map (t) -> Date.parseExact(t, "dd.MM.yyyy")?.toString "yyyy-MM-ddTHH:mm:ss.0Z"

  s2cTypes =
    'dictionary-set': (v) -> v
    'dictionary-many': (v) -> v.split(',')
    checkbox  : (v) -> v == "1"
    Bool      : (v) -> v
    Integer   : (v) -> v
    Double    : (v) -> v
    IdentList : (v) -> v
    Day       : s2cISO guiDayFormat
    UTCTime   : s2cISO guiUTCTimeFormat
    dictionary: (v) -> v
    date      : s2cDate("dd.MM.yyyy")
    datetime  : s2cDate("dd.MM.yyyy HH:mm")
    json      : s2cJson
    'interval-date': (v) -> v

  defaultc2s = (v) -> if _.isNull(v) then "" else String(v)
  c2s = (val, type) -> (c2sTypes[type] || defaultc2s)(val)
  s2c = (val, type) -> (s2cTypes[type] || _.identity)(val)

  mapObj = (mapper) -> (obj, types) ->
    r = {}
    r[k] = mapper(v, types[k]) for k, v of obj
    r

  modelTypes = (model) -> _.foldl model.fields, ((m, f) -> m[f.name] = f.type; m), {}

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
