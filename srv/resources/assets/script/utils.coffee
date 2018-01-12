{$, _, Finch} = require "carma/vendor"

mu          = require "carma/model/utils"
d           = require "carma/dictionaries"
{CrudQueue} = require "carma/sync/crud"

# jquery -> html(as string) conversion, with selected element
# FIXME bullshit, what if it is not wrapped with <div>?
$.fn.outerHTML = () -> $("<div>").append(@clone()).html()

# Synchronous JSON request
$.bgetJSON = (url, cb) ->
  $.ajax
    type     : 'GET'
    url      : url
    dataType : 'json'
    success  : cb
    async    : false

$.putJSON = (url, obj) ->
  $.ajax
    type        : "PUT"
    url         : url
    data        : JSON.stringify obj
    processData : false
    contentType : "application/json"

# Find VM of reference in a case by its view name.
findCaseOrReferenceVM = (view) ->
  kase = window.global.viewsWare["case-form"].knockVM
  if (view is "case-form")
    kase
  else
    _.find kase.servicesReference(), (svc) -> svc.view is view

# Find VM of a view, properly handling reference views or views of
# field groups. If the view name is "case-form", then return knockVM
# for case.
findVM = (view) ->
  if window.global.viewsWare["case-form"]
    vw = window.global.viewsWare[view]
    if vw and vw.parentView?
      # Find VM of a group rendered in a view.
      findCaseOrReferenceVM(vw.parentView)
    else
      findCaseOrReferenceVM(view)
  else
    window.global.viewsWare[view].knockVM

# make this global, still need to use this module as dependency
# to make sure that this functions will be loaded
window.el  = (id) -> document.getElementById(id) # FIXME global shit
window.$el = (id) -> $(el(id)) # FIXME global shit
# like _.has but for list
window.hasL = (lst, e) -> _.find(lst, (x) -> x == e) # FIXME global shit

# FIXME global shit
window.inlineSpinner = (el) ->
  $(el).addClass("inline-spinner").append(
    "<div class='bounce1'></div><div class='bounce2'></div><div class='bounce3'></div>"
  )

# FIXME global shit
window.getDictionary = (d) ->
  dict = window.global.dictionaries[d]
  return dict if dict
  return eval(d)

# Converts lists into objects. Pass either a single array of `[key, value]`
# pairs, or two parallel arrays of the same length -- one of keys, and one of
# the corresponding values.
_.object = (list, values) ->
  return {} if  _.isEmpty list
  if values
    _.object _.zip list, values
  else
    _.foldl list, ((a, [k, v]) -> a[k] = v; return a), {}

_.pairs = (obj) -> [k, v] for k, v of obj

# FIXME global shit
window.arrToObj = (key, val, f = _.identity) ->
  keys = if _.isFunction key then _.map val, key else _.pluck val, key
  _.object _.zip keys, (_.map val, f)

# FIXME doing tricky shit, fella? how about use it just from 'underscore'?
String.prototype.capitalize = -> @charAt(0).toUpperCase() + @slice(1)

bindRemove = (parent, field, cb) ->
  for i in parent["#{field}Reference"]()
    do (i) ->
      $("##{i['view']}")
        .parents('div.accordion-group')
        .first()
        .find('.icon.icon-remove')
        .click ->
          mu.removeReference(parent, field, i)
          bindRemove parent, field, cb
          cb(parent, field, i) if _.isFunction cb

modelsFromUrl = -> location.hash.match(/#(\w+)/)[1]

# Generate a random password of given length (default 10)
genPassword = (len) ->
  chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz123456789"
  temp = ""
  tlen = len || 10
  for i in [0..tlen]
    temp += chars.charAt Math.floor Math.random() * chars.length
  return temp

isMatch = (q, str) -> !!~String(str).toLowerCase().indexOf(q.toLowerCase())

allowedField = (r, f) ->
  return true unless r
  if r.allowed?
    _.contains(r.allowed, f.name) and not _.contains(r.forbidden, f.name)
  else
    not _.contains(r.forbidden, f.name)

kvmCheckMatch = (q, kvm, fieldsRestriction) ->
  v = for f in kvm._meta.model.fields when allowedField fieldsRestriction, f
    if f.type == "dictionary"
      isMatch(q, kvm["#{f.name}Local"]())
    else if _.contains(
      ["dictionary-many", "dictionary-set-int", "dictionary-set-text"],
      f.type)
      checkMatch(q, _.pluck(kvm["#{f.name}Locals"](), 'label'))
    else if f.type == "reference"
      _.any kvm["#{f.name}References"](), (k) -> kvmCheckMatch(q, k)
    else if f.type == "nested-model"
      _.any kvm["#{f.name}Nested"](), (k) -> kvmCheckMatch(q, k)
    else if f.type == "json" and f.meta?.jsonSchema == "dict-objects"
      _.any kvm["#{f.name}Objects"](),
            (k) -> _.any ['keyLocal', 'value', 'note'],
                        (s) -> checkMatch(q, k[s]?())
    else
      checkMatch(q, kvm[f.name]())

  _.any v

# deep check that anything in @val@ has @q@
checkMatch = (q, val) ->
  if _.isArray val
    _.any val, (a) -> checkMatch(q, a)
  else if _.isObject val
    _.any (checkMatch(q, v) for k, v of val), _.identity
  else
    !!~String(val).toLowerCase().indexOf(q.toLowerCase())
window.checkMatch = checkMatch # FIXME global shit

# Format a numeric value from seconds to hours and minutes
formatSec = (s) ->
  mins = Math.round(s / 60 % 60)
  hours = Math.floor(s / 3600 % 3600)
  if hours == 0
    "#{mins}м"
  else
    "#{hours}ч #{mins}м"

# Build a KnockVM for a model instance using standard queue
buildInstance = (modelName, id) ->

  # requiring it here to avoid recursive dependencies
  main = require "carma/model/main"

  main.buildKVM window.global.model(modelName),
    fetched: {id}
    queue: CrudQueue

newModelDict = (name, stringify, meta) ->
  new d.dicts.ModelDict
    dict: name
    meta:
      _.extend (meta || {}), {dictionaryStringify: stringify}

# Call a number if the CTI panel is available
ctiDial = (number) ->
  window.global.CTIPanel and \
  $("#cti").show() and \
  window.global.CTIPanel.instaDial(number)

module.exports = {

  makeAFuckingMess: ->
    # Replacement for old `build_global_fn` from 'utils'
    # FIXME The goal is to completely remove this historical big mistake.
    setGlobalShit = (name, module) -> window[name] = module[name]

    # Compatibility with old horrible shit.
    # From 'screens/call':
    setGlobalShit "reloadScreen",     require "carma/utils"
    # From 'screens/case':
    setGlobalShit "pickPartnerBlip",  require "carma/map"
    setGlobalShit "addService",       require "carma/screens/case"
    # From 'local':
    setGlobalShit "showComplex",      module.exports
    setGlobalShit "hideComplex",      module.exports
    setGlobalShit "doPick",           module.exports
    setGlobalShit "kdoPick",          module.exports
    setGlobalShit "edoPick",          module.exports
    setGlobalShit "focusField",       module.exports
    setGlobalShit "ctiDial",          module.exports

  ctiDial

  mkDataTable: (t, opts) ->
    defaults =
      sScrollY  : "500px"
      bPaginate : false
      oLanguage :
        sSearch      : "Фильтр"
        sInfoEmpty   : ""
        sZeroRecords : "Ничего не найдено"
        sInfo        : "Показаны записи с _START_ по _END_ (всего _TOTAL_)"

    defaults = $.extend(defaults, opts) if opts?

    t.dataTable defaults

  # Model method HTTP access point wrt redson location
  modelMethod: (modelName, method) -> "/_/#{modelName}/#{method}"

  getServiceDesc: (pid, service) ->
    md = newModelDict('ServiceInfo').source
    si = _.find (_.pluck md, '_e'), (info) ->
      info.program == pid and info.service == service
    si?.info or ""

  getProgramDesc: (pid, sid) ->
    return unless pid
    meta = {dictionaryLabel: 'help'}
    pvm = buildInstance('Program', pid) unless sid
    svm = buildInstance('SubProgram', sid) if sid
    if pvm?.pTypeLocal?()
      pType = "<span class=\"label label-info\">#{pvm.pTypeLocal()}</span>"
    else
      pType = null
    managers = _.pluck(pvm?.managersLocals?(), 'label')
    if managers? && !_.isEmpty(managers)
      manList = "<b>Менеджеры:</b> " + managers.join(', ')
    _.compact([pType, manList, pvm?.help?(), svm?.help?()]).
      join '<br />'

  # Scroll case field into view and focus
  focusField: (name) ->
    e = $("#main-container").find("[name=" + name + "]")[0]
    e.scrollIntoView()
    e.focus()

  findVM

  # Strip whitespace from string
  stripWs: (s) -> do (s) -> s.replace(/\s+/g, '')

  # Given a string of form "foo/bar", return object with fields
  # `view=foo` and `field=bar`. If input is of form "bar", then `view`
  # field is equal to defaultView.
  #
  # This is used to parse field references in meta annotations such as
  # targetCoords or targetAddr.
  splitFieldInView: (input, defaultView) ->
    chunks = input.split('/')
    if chunks.length > 1
      view_name = chunks[0]
      field_name = chunks[1]
    else
      view_name = defaultView
      field_name = chunks[0]

    obj =
      view: view_name
      field: field_name

  # Calculate delta between two timestamps, return formatted and
  # unformatted delta in a list. If second timestamp is omitted,
  # current time is used.
  timeFrom: (from, to) ->
    return null if _.isEmpty from
    from = new Date(from * 1000)
    if _.isEmpty to
      to = new Date()
    else
      to = new Date(to * 1000)
    delta = (to - from) / 1000
    return [formatSec delta, delta]

  # Format a numeric value from seconds to minutes
  formatSecToMin: (s) ->
    Math.round(s / 60) + "м"

  # Hide all views on center pane and show view for first reference
  # stored in <fieldName> of model loaded into <parentView> there
  showComplex: (parentView, fieldName) ->
    depViewName = window.global.viewsWare[parentView].depViews[fieldName][0]
    view = $el(depViewName)

    return if view.is(':visible')
    $(".complex-field").hide()

    view.show ->
      # "carma/map" depends on this module, cannot move "require" to top-level
      require("carma/map").initOSM(e, parentView) for e in view.find(".osMap")

  hideComplex: ->
    $(".complex-field").hide()
    $(".default-complex-field").show()

  # Dispatch on some picker type
  #
  # In templates, bind click to 'doPick({{meta.picker}}, ...,
  # event.target)' to call the appropriate picker.
  doPick: (pickType, args, elt) ->
    # "carma/map" depends on this module, cannot move "require" to top-level
    {geoPicker, reverseGeoPicker, mapPicker} = require "carma/map"

    pickers = {
      callPlease: (fieldName, el) ->
        viewName = mu.elementView($(el)).id
        kvm = findVM viewName
        return unless kvm
        number = kvm[fieldName]?()
        ctiDial number

      # Set a field to a new randomly generated password
      passwordPicker   : (fieldName, el) ->
        viewName = mu.elementView($(el)).id
        kvm = window.global.viewsWare[viewName].knockVM
        kvm[fieldName] genPassword()

      geoPicker
      reverseGeoPicker
      mapPicker
    }

    pickers[pickType] args, elt

  kdoPick: (pickType, args, k, e) ->
    doPick pickType, args, e.srcElement if e.ctrlKey and e.keyCode == k

  edoPick: (pickType, args, k, e) ->
    doPick pickType, args, e.srcElement if e.keyCode == k

  # Format a list of fields in a model to a tooltip with a list of
  # field labels
  reqFieldsTooltip: (kvm, fieldNames) ->
    labels = _.map fieldNames, (n) -> "#{mu.fieldNameToLabel(kvm)(n)}"
    labelsF = _.without labels, "undefined"
    "Доступно при заполнении полей: #{labelsF.join(', ')}"

  # True if some of named model fields are empty (not filled by the
  # user)
  someEmpty: (kvm, fieldNames) ->
    vals = _.map fieldNames, (n) -> kvm[n]?()
    empties = _.map vals, (e) -> e == "" || _.isNull e
    _.some empties

  # Select case actions with matching types and which are created for
  # this service. If types list is empty, match all action types.
  svcActions: (kase, svc, types) ->
    _.filter (kase['actionsList']?() || []),
      (a) -> (a.serviceId() == parseInt(svc.id())) &&
              (_.isEmpty(types) || _.contains types, a.type())

  checkAccordion: (e) ->
    acc = e.parents('.accordion-body') #.hasClass('in')
    return if acc.hasClass('in')
    acc.collapse('show')

  # Add selected-row class to a datatables row, remove this class from
  # all other rows in the same table
  highlightDataTableRow: (tr) ->
    tr.siblings(".selected-row").removeClass("selected-row")
    tr.addClass("selected-row")

  getWeather: (city, cb) ->
    url = "/#{city}"
    $.getJSON "/weather/#{city}", (data) -> cb(data)

  # Extract value of the first object from "dict-objects"-field JSON
  # contents with matching "key". If no such entries found, return
  # null.
  getKeyedJsonValue: (json, key) ->
    if json?.length > 0
      o = _.find json, (o) -> o.key == key
      if o?
        o.value
      else
        null

  # Set value of the first object from "dict-objects"-field JSON
  # contents with matching "key" (create it if no object matches key),
  # return new JSON string.
  setKeyedJsonValue: (json, key, value) ->
    newObj =
      key: key
      value: value
    if json?
      o = _.find json, (o) -> o.key == key
      if o?
        o.value = value
      else
        json = json.concat [newObj]
    else
      json = [newObj]
    json

  # Transform distance in meters to km
  formatDistance: (dist) -> Math.round ((parseInt dist) / 1000)

  # FIXME: remove this function definition
  # and correct module dependencies
  focusRef: mu.focusReference

  bindRemove

  toUnix: (d) -> Math.round(d.getTime() / 1000)

  # flip . setTimeout
  sTout: (wait, fn) -> setTimeout fn, wait

  repeat: (times, v) -> [1..times].map -> v

  modelsFromUrl

  reloadScreen: -> window.global.activeScreen.reload()

  checkMatch
  kvmCheckMatch

  parseUrlParams: (uri) ->
    fromUrlParams url.substring(url.indexOf('?') + 1)

  fromUrlParams: (str) ->
    dec = decodeURIComponent
    return {} if _.isEmpty str
    prms = {}
    for q in str.split('&')
      [n, v] = q.split '='
      prms[dec n] = dec v
    return prms

  getUrlParams: ->
    url = document.location.href
    @fromUrlParams url.split("?")[1]

  setUrlParams: (prms) ->
    [url, currPrms] = document.location.href.split("?")
    scr = window.location.href.match(/#(.*?)(\?|$)/)[1]
    nparams = $.extend (@fromUrlParams currPrms), prms
    enc = encodeURIComponent
    q = (("#{enc k}=#{enc v}" for k, v of nparams).join("&"))
    window.history.replaceState null, null, "/##{scr}?#{q}"

  inject: (dest, src) -> dest[k] = v for k, v of src when not dest[k]

  buildInstance

  newModelDict

  newComputedDict: (name, meta) ->
    new d.dicts.ComputedDict
      dict: name
      meta: meta

  # Convert pretty number to DeviceId for AVAYA
  displayedToInternal: (number) ->
    number.
      replace(/[^\+0-9]/g, "").
      replace(/^8/, "98").
      replace(/^\+7/, "98").
      replace(/^\+/, "9810").
      replace(/\+/, "")

  # Pretty-print ugly DeviceId from AVAYA
  internalToDisplayed: (number) ->
    number?.match(/\d+/)?[0]?.
      replace(/^(98|8|)(\d{10})$/, "\+7$2").
      replace(/^9810/, "+")

  createNewCall: (callData) ->
    $.notify "Создаём новый звонок…", {className: 'info'}

    # requiring it here to avoid recursive dependencies
    main = require "carma/model/main"

    cvm = main.buildKVM window.global.model('Call'),
      {fetched: callData, queue: CrudQueue}

    # Force saving
    cvm._meta.q.save null, true
    cvm.id.subscribe (id) -> Finch.navigate "call/#{cvm.id()}"

  # subset of d3.scale.category20 with dark colors removed
  palette: [
    '#aec7e8' # 1
    '#ffbb78' # 3
    '#98df8a' # 5
    '#ff9896' # 7
    '#c5b0d5' # 9
    '#c49c94' # 11
    '#e377c2' # 12
    '#f7b6d2' # 13
    '#c7c7c7' # 15
    '#bcbd22' # 16
    '#dbdb8d' # 17
    '#9edae5' # 19
    '#ff7f0e' # 2
  ]
}
