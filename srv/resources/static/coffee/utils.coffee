define ["map", "model/utils"], (map, mu) ->
  # jquery -> html(as string) conversion, with selected element
  jQuery.fn.outerHTML = () -> jQuery("<div>").append(this.clone()).html()

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
    si  = global.dictionaries['ServiceInfo'][program]?[service]
    si ?= global.dictionaries['ServiceInfo']['default']?[service]

  # Scroll case field into view and focus
  focusField: (name) ->
    e = $("#main-container").find("[name=" + name + "]")[0]
    e.scrollIntoView()
    e.focus()

  # Find VM of a view, properly handling reference views or views of
  # field groups. If the view name is "case-form", then return knockVM
  # for case.
  findVM: (view) ->
    vw = global.viewsWare[view]
    if vw and vw.parentView?
      # Find VM of a group rendered in a view.
      findCaseOrReferenceVM(vw.parentView)
    else
      findCaseOrReferenceVM(view)

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

  # Hide all views on center pane and show view for first reference
  # stored in <fieldName> of model loaded into <parentView> there
  showComplex: (parentView, fieldName) ->
    depViewName = global.viewsWare[parentView].depViews[fieldName][0]
    view = $el(depViewName)

    return if view.is(':visible')
    $(".complex-field").hide()

    view.show ->
      isDealerView = depViewName.match(/towDealer_partner-view/)
      isPartnerView = depViewName.match(/contractor_partner-view/)
      if isDealerView or isPartnerView
        require ["screens/case"], (c) -> c.initPartnerTables view, parentView

      map.initOSM(e, parentView) for e in view.find(".osMap")

  hideComplex: ->
    $(".complex-field").hide()
    $(".default-complex-field").show()

  # Dispatch on some picker type
  #
  # In templates, bind click to 'doPick({{meta.picker}}, ...,
  # event.target)' to call the appropriate picker.
  doPick: (pickType, args, elt) ->
    pickers =
      callPlease: (modelName) ->
        bb = global.viewsWare["call-form"].bbInstance
        number = bb.get(modelName)
        global.avayaPhone && global.avayaPhone.call(number)
      geoPicker        : map.geoPicker
      reverseGeoPicker : map.reverseGeoPicker
      mapPicker        : map.mapPicker
    pickers[pickType](args, elt)

  kdoPick: (pickType, args, k, e) ->
    doPick pickType, args, e.srcElement if e.ctrlKey and e.keyCode == k

  # FIXME: This could be a callback for main.js:saveInstance
  successfulSave: ->
    $span = $(this).siblings(".save-result")
    setTimeout((->
      $span.text("Сохранено успешно")
      $span.show()
      $span.fadeOut(2000))
    , 500)

  checkAccordion: (e) ->
    acc = e.parents('.accordion-body') #.hasClass('in')
    return if acc.hasClass('in')
    acc.collapse('show')

  getWeather: (city, cb) ->
    url = "/#{city}"
    $.getJSON "/weather/#{city}", (data) -> cb(data)

  focusRef: (kvm) ->
    e = $('#' + kvm['view'])
    e.parent().prev()[0].scrollIntoView()
    e.find('input')[0].focus()
    e.find('input').parents(".accordion-body").first().collapse('show')

  bindRemove: (parent, field, cb) ->
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

  bindDelete: (parent, field, cb) ->
    bindRemove parent, field, (p, f, kvm) ->
      deleteCb = (args...) -> cb(args) if _.isFunction cb
      $.ajax
        'type'     : 'DELETE'
        'url'      : "/_/#{kvm.modelName()}/#{kvm.id()}"
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
    v.split ','

  modelsFromUrl: -> window.location.hash.match(/#(.*)\/.*/)[1]

  reloadScreen: -> global.router.navigate modelsFromUrl(), { trigger: true }
