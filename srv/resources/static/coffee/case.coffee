# Case view (renders to #left, #center and #right as well)
this.setupCaseMain = (viewName, args) -> setupCaseModel viewName, args

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
    setCommentsHandler()

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
  for i of kvm when /.*Not$/.test(i) or i == 'actions'
    do (i) -> kvm[i].subscribe -> mbEnableActionResult(kvm)

mbEnableActionResult = (kvm) ->
  nots = (i for i of kvm when /.*Not$/.test i)
  if (_.any nots, (e) -> kvm[e]())
    $("[name=result]").attr('disabled', 'disabled')
    $("[name=result]").next().find("i").removeAttr("data-provide")
  else
    $("[name=result]").removeAttr 'disabled'
    $("[name=result]").next().find("i")
      .attr("data-provide", "typeahead-toggle")

setCommentsHandler = ->
  $("#case-comments-b").on 'click', ->
    i = $("#case-comments-i")
    return if _.isEmpty i.val()
    comment =
      date: (new Date()).toString('dd.MM.yyyy HH:mm')
      user: global.user.login
      comment: i.val()
    k = global.viewsWare['case-form'].knockVM
    if _.isEmpty k['comments']()
      k['comments'] [comment]
    else
      k['comments'] k['comments']().concat comment
    i.val("")


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
  $.getJSON "/callsByPhone/#{phone}", (calls) ->
    $.getJSON "/actionsFor/#{knockVM.id()}", (actions) ->
      st.fnClearTable()
      dict = global.dictValueCache

      for i of calls
        obj = calls[i]
        callDate = if obj.callDate
            new Date(obj.callDate * 1000).toString("dd.MM.yyyy HH:mm")
          else
            ''
        comment = []
        wazzup  = dict.Wazzup[obj.wazzup] || obj.wazzup || ''
        comment.push("Что случилось: #{wazzup}") if wazzup
        callType = dict.CallerTypes[obj.callType] || obj.callType || ''
        comment.push("Тип звонка: #{callType}") if callType
        comment.push("ФИО: #{obj.callerName_name}") if obj.callerName_name
        city = dict['DealerCities'][obj.city]
        comment.push("Город: #{city}") if city
        program = global.dictionaries['Programs'][obj.program]
        comment.push("Программа: #{program}") if program
        make = dict['CarMakers'][obj.make]
        comment.push("Марка: #{make}") if make
        model = dict['CarModels'][obj.model]
        comment.push("Модель: #{model}") if model
        comment.push("Сотрудник РАМК: #{obj.callTaker}") if obj.callTaker
        row = [ callDate
              , obj.callTaker || ''
              , "звонок"
              , comment.join("<br/>")
              , ''
              ]

        st.fnAddData(row)

      for r in actions
        result = dict.ActionResults[r.result] or ''
        name = dict.ActionNames[r.name] or ''
        aTo  = global.dictValueCache['users'][r.assignedTo] or
               r.assignedTo or ''
        row = [ new Date(r.closeTime * 1000).toString("dd.MM.yyyy HH:mm")
              , aTo
              , name
              , r.comment or ''
              , result ]

        st.fnAddData(row)

      return if _.isEmpty knockVM['comments']()
      for c in knockVM['comments']()
        st.fnAddData [ c.date
                     , global.dictValueCache['users'][c.user] || ''
                     , "Комментарий"
                     , c.comment
                     , ""
                     ]

this.removeCaseMain = ->
  $("body").off "change.input"

# get partners and show them in table
# this is called from local.coffe:showCase
this.initPartnerTables = ($view,parentView) ->
  m = $view[0].id.match(/(\w*)_partner-view/)
  partnerType = m[1]
  table = $view.find("table##{partnerType}_partnerTable")
  kase = global.viewsWare["case-form"].knockVM
  svc = findCaseOrReferenceVM(parentView)
  # this options for datatable will hide priorities columns for dealer table
  tblOpts = if partnerType is "contractor"
              {}
           else
              { aoColumns: repeat(5, null).concat(repeat(3, { bVisible: false})) }

  unless table.hasClass("dataTable")
    mkDataTable table, tblOpts
    table.on "click.datatable", "tr", ->
      name = this.children[0].innerText
      city = this.children[1].innerText
      addr = this.children[2].innerText
      svc["#{partnerType}_partner"](name)
      svc["#{partnerType}_address"]("#{city}, #{addr}")
      svc["#{partnerType}_partnerId"]($(this).attr('partnerid'))

  table = table.dataTable()
  # hope that contractor_partner is the only partner
  dealer = if partnerType is "contractor" then 0 else 1
  select = ["isActive=1", "isDealer=#{dealer}"]
  select.push("city=#{kase.cityLocal()}") if kase.cityLocal()
  url    = if partnerType is "contractor"
              "/partnersFor/#{svc.modelName()}?#{select.join('&')}"
           else
              "/allPartners?#{select.join('&')}"

  $.getJSON url, (objs) ->
    # Store partner cache for use with maps
    cache = {}
    rows = for p in objs
      p.name = p.name.trim()
      cache[p.id] = p
      [p.name        || '',
       p.city        || '',
       p.addrDeFacto || '',
       p.phone1      || '',
       p.workingTime || '',
       p.priority1   || '',
       p.priority2   || '',
       p.priority3   || '',
       p.id]
    # this last id will never be shown, but I need this, to add
    # partnerid as attribute of the row to pass it then to
    # the service kvm
    table.data("cache", cache)
    table.fnClearTable()
    r = table.fnAddData(rows)
    n = table.fnSettings().aoData[ r[0] ]
    # this will set partnerid attribute to each row
    # FIXME: find better way to do this
    for i in r
      s  = table.fnSettings().aoData[ i ]
      tr = s.nTr
      id = s._aData[8]
      $(tr).attr('partnerid', "partner:#{id}")

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
  knockVM['comments'].subscribe fillEventsHistory(knockVM)

this.partnerOptsHook = (i, knockVM) ->
  knockVM['contractor_partner'].subscribe (n) ->
    return unless knockVM['view']
    v = global.viewsWare[knockVM['view']].depViews['cost_counted'][0]
    $("##{v}").find(".add-opt-btn").remove()
    model = knockVM.modelName()
    sTout 1000, ->
      $.getJSON "/opts/#{knockVM.modelName()}/#{knockVM.id()}", (opts)->
        return if _.isEmpty opts
        tr = Mustache.render(
              $('#tarif-opt-sel-template').html(),
              opts:
                for i in opts
                  { id: i.id
                  , optionName: (i.optionName || "Тарифная опция")}
        )
        $("##{v}").children().last().after(tr)
        $("##{v}").find('.reload').on 'click.reloadCountedCost', ->
          r = global.viewsWare['case-form'].knockVM['servicesReference']()
          o.model().fetch() for o in r
        $("##{v}").find('.add').on 'click.addTarif', ->
          s = $("##{v}").find("select")
          return if _.isEmpty s
          o = _.find opts, (opt) -> "#{opt.id}" == s.val()
          addReference knockVM, 'cost_serviceTarifOptions',
            modelName: "cost_serviceTarifOption"
            args     :
              optionName   : o.optionName
              tarifOptionId: "tarifOption:#{o.id}"
            ->
              bindDelete knockVM, 'cost_serviceTarifOptions'
              r = knockVM['cost_serviceTarifOptionsReference']()
              $("##{(_.last r)['view']}").parent().collapse("show")
        bindDelete knockVM, 'cost_serviceTarifOptions'

this.srvOptUpd = (instance, knockVM) ->
  knockVM['payType'].subscribe (n) ->
    sTout 500, ->
      for o in knockVM['cost_serviceTarifOptionsReference']()
        do (o) ->
          o.model().fetch()

this.costsMark = (instance, knockVM) ->
  knockVM['marginalCost'].subscribe -> mbMark()

  knockVM['cost_counted'].subscribe -> mbMark()
  mbMark = ->
    v = knockVM.view
    mc = $("##{v}").find('[name=marginalCost]').parents('.control-group')
    cc = $("##{v}").find('[name=cost_counted]').parents('.control-group')
    mf = parseFloat(knockVM['marginalCost']())
    cf = parseFloat(knockVM['cost_counted']())
    if mf < cf
      mc.addClass('error')
      cc.addClass('error')
    else
      mc.removeClass('error')
      cc.removeClass('error')
