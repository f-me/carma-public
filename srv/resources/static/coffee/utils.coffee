define ["model/utils", "lib/ident/role"], (mu, role) ->
  # jquery -> html(as string) conversion, with selected element
  jQuery.fn.outerHTML = () -> jQuery("<div>").append(this.clone()).html()

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
    kase = global.viewsWare["case-form"].knockVM
    if (view is "case-form")
      kase
    else
      _.find kase.servicesReference(), (svc) -> svc.view is view

  # make this global, still need to use this module as dependency
  # to make sure that this functions will be loaded
  window.el  = (id) -> document.getElementById(id)
  window.$el = (id) -> $(el(id))
  # like _.has but for list
  window.hasL = (lst, e) -> _.find(lst, (x) -> x == e)

  window.successfulSave = ->
    $span = $(this).siblings(".save-result")
    setTimeout((->
      $span.text("Сохранено успешно")
      $span.show()
      $span.fadeOut(2000))
    , 500)

  window.getDictionary = (d) ->
    dict = global.dictionaries[d]
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

  window.arrToObj = (key, val, f = _.identity) ->
    keys = if _.isFunction key then _.map val, key else _.pluck val, key
    _.object _.zip keys, (_.map val, f)

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

  # args: id - id of datatable element
  # to: id of element where href will be set
  window.dt2csv = (id, to) ->
    h = $($("##{id}").dataTable().fnSettings().nTHead)
          .find('th').map (i,e) -> $(e).text()
    d = $("##{id}").dataTable()
    m = d.$("tr", {filter: 'applied'})
         .map (i,e) -> $(e).children()
                           .map (i,e) -> $(e).text()
    head = ($.makeArray(h).join ';') + "\n"
    s = ($.map m, (e, i) -> $.makeArray(e).join(';')).join "\n"
    $("##{to}").attr 'href',
      " data:application/octet-stream
      ; base64
      , #{Base64.encode('\uFEFF' + head + s)}"
    s

  modelsFromUrl = -> window.location.hash.match(/#(.*)\/.*/)[1]

  # Generate a random password of given length (default 10)
  genPassword = (len) ->
    chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz123456789"
    temp = ""
    tlen = len || 10
    for i in [0..tlen]
      temp += chars.charAt Math.floor Math.random() * chars.length
    return temp

  isMatch = (q, str) -> !!~String(str).toLowerCase().indexOf(q.toLowerCase())

  kvmCheckMatch = (q, kvm) ->
    v = for f in kvm._meta.model.fields
      if f.type == "dictionary"
        isMatch(q, kvm["#{f.name}Local"]())
      else if f.type == "dictionary-many"
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
  window.checkMatch = checkMatch

  # Format a numeric value from seconds to hours and minutes
  formatSec = (s) ->
    mins = Math.round(s / 60 % 60)
    hours = Math.floor(s / 3600 % 3600)
    if hours == 0
      "#{mins}м"
    else
      "#{hours}ч #{mins}м"

  # Return true if user may access case/service-related actions
  canReadActions: () ->
    _.some (global.model "case").fields, (f) -> f.name == 'actions'

  findCaseOrReferenceVM: findCaseOrReferenceVM

  # build global function from local to module one
  # function should belong to first dependency
  build_global_fn: (name, deps) ->
    window[name] = ->
      args = arguments;
      require deps, (dep) -> dep[name].apply(this, args)

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

  getServiceDesc: (program, service) ->
    if p = @findProgram program
      si = _.find global.dictionaries['ServiceInfo'].entries, (info) ->
        info.program == p.id and info.service == service
      si?.info or ""
    else
      ""

  getProgramDesc: (program) ->
    if p = @findProgram program
      pi = _.find global.dictionaries['ProgramInfo'].entries, (info) ->
        info.program == p.id
      pi?.info or ""
    else
      ""

  findProgram: (name) ->
    _.find global.dictionaries['Programs'].entries, (program) ->
      program.value == name

  # Scroll case field into view and focus
  focusField: (name) ->
    e = $("#main-container").find("[name=" + name + "]")[0]
    e.scrollIntoView()
    e.focus()

  # Find VM of a view, properly handling reference views or views of
  # field groups. If the view name is "case-form", then return knockVM
  # for case.
  findVM: (view) ->
    if global.viewsWare["case-form"]
      vw = global.viewsWare[view]
      if vw and vw.parentView?
        # Find VM of a group rendered in a view.
        findCaseOrReferenceVM(vw.parentView)
      else
        findCaseOrReferenceVM(view)
    else
      global.viewsWare[view].knockVM

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
    depViewName = global.viewsWare[parentView].depViews[fieldName][0]
    view = $el(depViewName)

    return if view.is(':visible')
    $(".complex-field").hide()

    view.show ->
      require ["map"], (map) ->
        map.initOSM(e, parentView) for e in view.find(".osMap")

  hideComplex: ->
    $(".complex-field").hide()
    $(".default-complex-field").show()

  # Dispatch on some picker type
  #
  # In templates, bind click to 'doPick({{meta.picker}}, ...,
  # event.target)' to call the appropriate picker.
  doPick: (pickType, args, elt) ->
    require ["map"], (map) ->
      pickers =
        callPlease: (modelName) ->
          kvm = global.viewsWare["call-form"].knockVM
          number = kvm[modelName]?()
          global.avayaPhone && global.avayaPhone.call(number)
        # Set a field to a new randomly generated password
        passwordPicker   : (fieldName, el) ->
          viewName = mu.elementView($(el)).id
          kvm = global.viewsWare[viewName].knockVM
          kvm[fieldName] genPassword()
        geoPicker        : map.geoPicker
        reverseGeoPicker : map.reverseGeoPicker
        mapPicker        : map.mapPicker
      pickers[pickType](args, elt)

  kdoPick: (pickType, args, k, e) ->
    doPick pickType, args, e.srcElement if e.ctrlKey and e.keyCode == k


  # FIXME: This could be a callback for main.js:saveInstance
  successfulSave: successfulSave

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

  bindRemove: bindRemove

  bindDelete: (parent, field, cb) ->
    bindRemove parent, field, (p, f, kvm) ->
      deleteCb = (args...) -> cb(args) if _.isFunction cb
      $.ajax
        'type'     : 'DELETE'
        'url'      : "/_/#{kvm._meta.model.name}/#{kvm.id()}"
        'success'  : -> deleteCb
        'error'    : (xhr) ->
          if xhr.status == 404
            deleteCb(d.acc())
          else
            alert 'error'

  toUnix: (d) -> Math.round(d.getTime() / 1000)

  # flip . setTimeout
  sTout: (wait, fn) -> setTimeout fn, wait

  repeat: (times, v) -> [1..times].map -> v

  splitVals: (v) ->
    return [] if not v or v == ""
    # some times we send them as array right from backend
    return v if _.isArray v
    v.split ','

  modelsFromUrl: modelsFromUrl

  reloadScreen: -> global.router.navigate modelsFromUrl(), { trigger: true }

  checkMatch: checkMatch
  kvmCheckMatch: kvmCheckMatch

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
    scr = global.router.current()
    nparams = $.extend (@fromUrlParams currPrms), prms
    enc = encodeURIComponent
    q = (("#{enc k}=#{enc v}" for k, v of nparams).join("&"))
    global.router.navigate "#{scr}?#{q}"