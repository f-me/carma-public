# Case view (renders to #left, #center and #right as well)
this.setupCaseMain = (viewName, args) ->
  $.getJSON "/all/partner", (objs) ->
    o = _.filter objs, (p) -> not _.isEmpty p.name
    global.partners = o
    global.dictionaries.partners =
      entries:
        for p in o
           n = p.name.replace('\r', '')
           {value: n, label: n}
    global.dictValueCache.partners = {}
    global.dictLabelCache.partners = {}
    for d in global.dictionaries.partners.entries
      # console.log 'setting', d.value, d.label
      global.dictLabelCache.partners[d.label] = d.value
      global.dictValueCache.partners[d.value] = d.label
    global.dictionaries.partners1 =
      entries: ({value: p.id, label: p.name.replace('\r', '')} for p in o)
    global.dictValueCache.partners1 = {}
    global.dictLabelCache.partners1 = {}
    for d in global.dictionaries.partners1.entries
      global.dictLabelCache.partners1[d.label] = d.value
      global.dictValueCache.partners1[d.value] = d.label

    setupCaseModel viewName, args

setupCaseModel = (viewName, args) ->

  # Default values
  # FIXME: User's name and creation date are better to be assigned by
  # the server.


  # Render list of required fields in right pane
  #
  # bbInstance is available only after model has been loaded. The
  # only way to execute custom code inside modelSetup is using
  # fetchCb option. By the time slotsee's are bound, fetchCb may
  # not have been called yet, thus we explicitly use applyBindings
  # here.
  fetchCb =  () ->
    instance = global.viewsWare[viewName].bbInstance
    ctx =
      "fields": _.map(instance.requiredFields, (f) -> instance.fieldHash[f])

    $("#empty-fields-placeholder").html(
      Mustache.render($("#empty-fields-template").html(), ctx))

    ko.applyBindings(global.viewsWare[viewName].knockVM,
                     el("empty-fields"))

  modelSetup("case") viewName, args,
                     permEl       : "case-permissions"
                     focusClass   : "focusable"
                     slotsee      : ["case-number"]
                     groupsForest : "center"
                     fetchCb      : fetchCb

  # Render service picker
  #
  # We use Bootstrap's glyphs if "icon" key is set in dictionary
  # entry.
  $("#service-picker-container").html(
    Mustache.render($("#service-picker-template").html(),
                    {dictionary: global.dictionaries["Services"]}))

  $("body").on("change.input", ".redirectOnChange", () ->
      setTimeout(( -> window.location.hash = "back"), 500))

  mkDataTable $('#call-searchtable')
  setupHotkeys()
  kvm = global.viewsWare[viewName].knockVM
  # addOptsBlock kvm
  kvm['servicesReference'].subscribe (n) ->
    addOptsBlock kvm
    # for s in n
    #   console.log s['view'], global.viewsWare[s['view']]
    #   console.log global.viewsWare[s['view']].depViews['cost_countedCost']
    #   v = global.viewsWare[s['view']].depViews['cost_countedCost']
    #   console.log $("##{v[0]}").find('.control-group').last() if v


  # drawAddTarif global.viewsWare[viewName].knockVM

# Top-level wrapper for storeService
this.addService = (name) ->
  kvm = global.viewsWare["case-form"].knockVM
  addReference kvm,
               'services',
               { modelName : name },
               (k) ->
                  e = $('#' + k['view'])
                  e.parent().prev()[0].scrollIntoView()
                  e.find('input')[0].focus()
  addOptsBlock kvm

this.makeCase = () ->
  v = global.viewsWare['call-form'].knockVM
  args =
    contact_name:   v['callerName_name']()
    contact_phone1: v['callerName_phone1']()
    contact_phone2: v['callerName_phone2']()
    contact_phone3: v['callerName_phone3']()
    contact_phone4: v['callerName_phone4']()
    contact_email:  v['callerName_email']()
    contact_contactOwner: v['callerName_contactOwner']()
    contact_ownerName:    v['callerName_ownerName']()
    contact_ownerPhone1:  v['callerName_ownerPhone1']()
    contact_ownerPhone2:  v['callerName_ownerPhone2']()
    contact_ownerPhone3:  v['callerName_ownerPhone3']()
    contact_ownerPhone4:  v['callerName_ownerPhone4']()
    contact_ownerEmail:   v['callerName_ownerEmail']()
    program:        v['program']()
    city:           v['city']()
    car_make:       v['make']()
    comment:        v['wazzup']()
    callTaker: global.user.meta.realName
  buildNewModel 'case', args, {},
    (a, b, k) ->
      global.router.navigate("case/#{k.id()}", { trigger: true })


fillEventsHistory = (knockVM) -> ->
  t = $("#call-searchtable")
  st = t.dataTable()
  # return if table template is not yet rendered
  return unless $("#call-searchtable")[0]

  phone = knockVM['contact_phone1']()
  $.getJSON "/ix/callsByPhone/#{phone}", (calls) ->
    $.getJSON "/actionsFor/#{knockVM.id()}", (actions) ->
      st.fnClearTable()
      dict = global.dictValueCache

      for i of calls
        obj = calls[i]
        continue if obj.id.length > 10
        wazzup  = dict.Wazzup[obj.wazzup] || obj.wazzup || ''
        wazzupMsg  = "Что случилось: #{wazzup}"
        whocall = dict.CallerTypes[obj.callerType] || obj.callerType || ''
        whocallMsg = "Кто звонил: #{whocall}"
        callDate = if obj.callDate
            new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm")
          else
            ''
        callType = dict.CallerTypes[obj.callType] || obj.callType || ''
        callTypeMsg = "Тип звонка: #{callType}"
        row = [ callDate
              , obj.callTaker || ''
              , "звонок"
              , "#{wazzupMsg}, #{whocallMsg}, #{callTypeMsg}"
              , ''
              ]

        st.fnAddData(row)

      for r in actions
        duetime = if r.duetime
            new Date(r.duetime * 1000).toString("dd.MM.yyyy HH:mm")
          else
            ''
        result = dict.ActionResults[r.result] or ''
        name = dict.ActionNames[r.name] or ''
        row = [ duetime , r.assignedTo or '', name , r.comment or '', result ]
        st.fnAddData(row)

# render checkboxes, trueChecks contains list with names,
# tha should be rendered as checked
renderChecks = (name, trueChecks) ->
  str = ""
  tpl = $("#check-list-item-template").html()
  if _.has(global.checks, name)
    for n of global.checks[name]["checks"]
      check = global.checks[name]["checks"][n]
      v = $(Mustache.render(tpl, check))
      if hasL(trueChecks, check.name)
        v.find('input:checkbox').attr('checked', true)
      str += v.outerHTML()
  return str


# try to render checkboxes, if check is found, then
# make request to candibober, and render checkboxes
# with 'renderChecks'
maybeRenderChecks = (e, instance) ->
  str = ""
  tpl = $("#check-list-item-template").html()
  name = instance.get(e.data('depends'))
  if _.has(global.checks, name)
    h = {}
    h[instance.name] =
      'model' : instance.name
      'id'    : instance.id

    $.ajax
      'dataType' : 'json'
      'type'     : 'POST'
      'url'      : '/candibober/check/' + name
      'data'     : JSON.stringify(h)
      'success'  : (data) -> e.html(renderChecks(name, data.true))
      'error'    : -> e.html(renderChecks(name, []))

# Update checks information when parent fields change
this.candiboberHook = (elName) ->
  instance = global.viewsWare[elName].bbInstance
  $el(elName).find("[data-provide=checklist]").each(
    (i) ->
      ((e) ->
        # Grab value of instance field specified in
        # data-depends and render associated checks
        instance.bind("change:" + e.data("depends"),
                      ((v) -> maybeRenderChecks(e, instance)))
       )($(this))
      )

#############################################################################
# kb hooks

this.caseDescsKbHook = (instance, knockVM) ->
  knockVM['servicesDescs'] = ko.computed
    read: ->
      p = knockVM['program']()
      s = knockVM['servicesReference']()
      return [] unless p?
      _.chain(s).map((x) -> mkServicesDescs(p,x)).compact().value()
  knockVM['programDesc'] = ko.computed
    read: ->
      global.dictionaries['ProgramInfo'][knockVM['program']()]

this.caseEventsHistoryKbHook = (instance, knockVM) ->
  knockVM['contact_phone1'].subscribe fillEventsHistory(knockVM)
  knockVM['actions'].subscribe fillEventsHistory(knockVM)

this.rentPartnerOptsHook = (i, knockVM) ->
  knockVM['contractor_partner'].subscribe (v) ->
    model = knockVM.modelName()
    console.log "changed '#{v}'"
    v1 = global.dictLabelCache.partners1[v.trim()]
    console.log v1
    if v1 and id = v1.split(':')?[1]
      buildNewModel "partner", {id: id}, {}, (m,mo,kvm)->
        sTout 1000, ->
          console.log kvm
          global.kvm = kvm
          services = kvm.servicesReference()
          filtered = _.filter(services, (s) -> s.serviceName() == model)
          console.log 'filtered', filtered
          opts = filtered[0].tarifOptionsReference() unless _.isEmpty filtered
          global.o = opts
          return if _.isEmpty opts
          opts1 = for i in opts
            {id: i.id(), optionName: (i.optionName() || "Тарифная опция")}
          tr = Mustache.render $('#tarif-opt-sel-template').html(),
                opts: opts1
          console.log tr
          console.log opts1
          $("##{tarifSel(knockVM)}").children().last().before(tr)
          console.log tarifOpts(knockVM)
          ko.applyBindings(knockVM, $("##{tarifOpts(knockVM)}")[0])
          # bindRemoveOption knockVM
          global.services = {} unless global.services
          global.services[knockVM.id()] = opts
          console.log '!!!!', knockVM.modelName(), knockVM.id()
          # addOption knockVM

this.addOptsBlock = (kvm) ->
  console.log 'add opts'
  for k in kvm['servicesReference']() when getCc(k)
    t = Mustache.render $('#tarif-opts-template').html(),
          modelName: k.modelName()
          cid: k.model().cid
    v = getCc k
    $("##{v}").find('.control-group').last().after(t)
    console.log 'addOption'
    addOption kvm, k
    # ko.applyBindings k.option $("##{tarifSel k}")

getCc = (kvm) ->
  v = global.viewsWare[kvm['view']].depViews['cost_countedCost']
  # $("##{v[0]}") if v
  return v[0] if v

tarifSel  = (kvm) -> "#{kvm.modelName()}-#{kvm.model().cid}-tarif-select"
tarifOpts = (kvm) -> "#{kvm.modelName()}-#{kvm.model().cid}-tarif-opts"

this.addOption = (p, kvm) ->
  console.log 'sel acc', "##{tarifSel(kvm)} > input"
  console.log 'sel dom', $("##{tarifSel(kvm)} > input")

  $("##{tarifSel(kvm)} > input").on 'click.addTarif', ->
    s = $("##{tarifSel(kvm)} > select")
    return if _.isEmpty s
    console.log s.val(), p.id(), p.modelName(), kvm.id(), kvm.modelName()
    console.log global.services[kvm.id()]
    o = _.find global.services[kvm.id()], (opt) -> opt.id() == s.val()
    console.log 'o', o, o.id(), o.optionName()
    addReference kvm, 'service_tarifOptions',
      modelName: "service_tarifOption"

this.bindRemoveOption = (el, kvm) ->
  parent = $(el).parent().parent().data().knockVM
  removeReference parent, 'service_tarifOptions', kvm
  deleteCb = (r) ->
    console.error r unless _.isEmpty r
    kb.vmRelease(kvm)
  $.ajax
    'type'     : 'DELETE'
    'url'      : "/_/#{kvm.modelName()}/#{kvm.id()}"
    'success'  : deleteCb
    'error'    : (xhr) ->
      if xhr.status == 404
        deleteCb(d.acc())
      else
        alert 'error'
