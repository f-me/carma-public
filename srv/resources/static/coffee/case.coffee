# Case view (renders to #left, #center and #right as well)
this.setupCaseMain = (viewName, args) ->

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

# Top-level wrapper for storeService
this.addService = (name) ->
  addReference global.viewsWare["case-form"].knockVM,
               'services',
               { modelName : name },
               (k) ->
                  e = $('#' + k['view'])
                  e.parent().prev()[0].scrollIntoView()
                  e.find('input')[0].focus()

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
        callerName = "ФИО: #{obj.callerName_name || ''}"
        city = dict['DealerCities'][obj.city]
        cityMsg = "Город: #{city || ''}"
        program = global.dictionaries['ProgramInfo'][obj.program]
        programMsg = "Программа: #{program || ''}"
        make = dict['CarMakers'][obj.make]
        makeMsg = "Марка: #{make || ''}"
        model = dict['CarModels'][obj.model]
        modelMsg = "Модель: #{model || ''}"
        callTaker = "Сотрудник РАМК: #{obj.callTaker || ''}"
        callDate = if obj.callDate
            new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm")
          else
            ''
        callType = dict.CallerTypes[obj.callType] || obj.callType || ''
        callTypeMsg = "Тип звонка: #{callType}"
        row = [ callDate
              , obj.callTaker || ''
              , "звонок"
              , "#{wazzupMsg},<br />
                 #{callTypeMsg},<br />
                 #{callerName},<br />
                 #{cityMsg},<br />
                 #{programMsg},<br />
                 #{makeMsg},<br />
                 #{modelMsg},<br />
                 #{callTaker}"
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

this.removeCaseMain = ->
  $("body").off "change.input"

# get partners and show them in table
# this is called from local.coffe:showCase
this.initPartnerTables = ($view,parentView) ->
  m = $view[0].id.match(/(\w*)_partner-view/)
  partnerType = m[1]
  table = $view.find("table##{partnerType}_partnerTable")
  kase = global.viewsWare["case-form"].knockVM
  svc = _.find(
          kase.servicesReference(),
          (svc) -> svc.view is parentView)

  unless table.hasClass("dataTable")
    mkDataTable(table)
    table.on "click.datatable", "tr", ->
      name = this.children[0].innerText
      city = this.children[1].innerText
      addr = this.children[2].innerText
      svc["#{partnerType}_partner"](name)
      svc["#{partnerType}_address"]("#{city}, #{addr}")


  table = table.dataTable()
  fields = "name,city,addrDeFacto,phone1,workingTime"
  dealer = if partnerType is "towDealer" then 1 else 0
  select = "city == #{kase.cityLocal()},isActive == 1,isDealer == #{dealer}"
  $.getJSON "/all/partner?fields=#{fields}&select=#{select}", (objs) ->
    rows = for p in objs
      [p.name        || '',
       p.city        || '',
       p.addrDeFacto || '',
       p.phone1      || '',
       p.workingTime || '']
    table.fnClearTable()
    table.fnAddData(rows)

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
