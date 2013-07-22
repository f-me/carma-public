define [ "utils"
       , "model/main"
       , "sync/dipq"
       , "text!tpl/screens/partnersSearch.html"
       ], (utils, m, sync, tpl) ->

  storeKey = 'partnersSearch'
  subName = (fld, model, id) ->
    if id
      "search_#{model}:#{id}_#{fld}"
    else
      "search_#{model}_#{fld}"

  open = (prm) -> window.open("/#partnersSearch/#{prm}", "_blank")

  model =
    name: "partnerSearch"
    title: "Экран поиска партнеров"
    fields: [
      { name: "search"
      , meta: { label: "Поиск" }
      },
      { name: "city"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "DealerCities"
          label: "Город"
      },
      { name: "make"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "CarMakers"
          label: "Марка"
      },
      { name: "services"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Services"
          label: "Услуги"
      },
      { name: "priority2"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Priorities"
          dictionaryType: "ComputedDict"
          bounded: true
          label: "ПБГ"
      },
      { name: "priority3"
      , type: "dictionary-many"
      , meta:
          dictionaryName: "Priorities"
          dictionaryType: "ComputedDict"
          bounded: true
          label: "ПНГ"
      },
      { name: "isDealer"
      , type: "checkbox"
      , meta: { label: "Дилер" }
      },
      { name: "mobilePartner"
      , type: "checkbox"
      , meta: { label: "Мобильный партнер" }
      },
      { name: "workNow"
      , type: "checkbox"
      , meta: { label: "Работают сейчас" }
      }
      ]

  # fh is just mapping from field name to field
  fh = {}
  fh[f.name] = f for f in model.fields

  mkPartials = (ps) ->
    $("<script class='partial' id='#{k}'>").html(v)[0].outerHTML for k,v of ps

  partialize = (ps) -> mkPartials(ps).join('')

  txt = $("#text-field-template").html()
  md  = $("#dictionary-many-field-template").html()
  cb  = $("#checkbox-field-template").html()
  srch = Mustache.render txt, fh['search']
  city = Mustache.render md,  fh['city']
  make = Mustache.render md,  fh['make']
  srvs = Mustache.render md,  fh['services']
  pr2  = Mustache.render md,  fh['priority2']
  pr3  = Mustache.render md,  fh['priority3']
  dlr  = Mustache.render cb,  fh['isDealer']
  mbp  = Mustache.render cb,  fh['mobilePartner']
  wn   = Mustache.render cb,  fh['workNow']

  srvLab = (val) -> window.global.dictValueCache.Services[val] || val

  setupCase = (kvm, ctx) ->
    kase = ctx['case'].data
    {id, data} = ctx['service']
    srvName = id.split(':')[0]
    kaseKVM = m.buildKVM global.models['case'], '', kase
    srvKVM  = m.buildKVM global.models[srvName], '', data
    kvm['city'](kaseKVM.city())
    kvm['make'](kaseKVM.car_make())
    kvm['services'](srvName)
    kvm['caseInfo'] = """
    <ul class='unstyled'>
      <li>
        <b>Кто звонил:</b> #{kaseKVM.contact_name()} #{kaseKVM.contact_phone1()}
      </li>
      <li> <b>Номер кеса:</b> #{kaseKVM.id()} </li>
      <li> <b>Адрес кейса:</b> #{kaseKVM.caseAddress_address()}</li>
      <li> <b>Название программы: </b> #{kaseKVM.programLocal()} </li>
      <li> <b> Марка: </b> #{kaseKVM.car_makeLocal()}</li>
      <li> <b> Модель: </b> #{kaseKVM.car_modelLocal()}</li>
      <li> <b> Госномер: </b> #{kaseKVM.car_plateNum()}</li>
      <li> <b> Цвет: </b> #{kaseKVM.car_colorLocal()}</li>
      <li> <b> VIN:</b> #{kaseKVM.car_vin()}</li>
      <li> <b> Тип оплаты:</b> #{srvKVM.payTypeLocal()}</li>
    </ul>
    """
    kvm['selectPartner'] = (partner, ev) ->
      kvm['selectedPartner'](partner.id)
      global.pubSub.pub subName(ctx.field, id), partner

  loadContext = (kvm, args) ->
    return unless args?.model
    return unless localStorage['partnersSearch']
    ctx = JSON.parse localStorage['partnersSearch']
    switch args.model
      when "case"
        setupCase kvm, ctx
      when "call"
        kvm['city'](ctx.city)
        kvm['make'](ctx.carMake)

    # cleanup localstore
    localStorage.removeItem 'partnersSearch'

  resizeResults = ->
    t = $("#search-result").offset().top
    w = $(window).height()
    $("#search-result").height(w-t-10)

  constructor: (view, args) ->
    # remove padding so blank space after removing navbar can be used
    $('body').css('padding-top', '0px')
    $(".navbar").hide()

    kvm = m.buildKVM(model, "partnersSearch-content")
    q = new sync.DipQueue(kvm, model)
    kvm._meta.q = q
    kvm['searchResults'] = ko.observable()
    kvm['searchH'] = ko.computed ->
      s = kvm['searchResults']()
      return [] unless s
      r = {}
      for v in s
        r[v.id] ?= v
        r[v.id]['services'] ?= []
        r[v.id]['services'].push
          name     : srvLab v.serviceName
          priority2: v.priority2
          priority3: v.priority3
      r

    kvm['searchProcessed'] = ko.computed ->
      for k, v of kvm['searchH']()
        v.services = _.sortBy v.services, (v) -> [v.priority2, v.priority3]
        v
    kvm['selectedPartner'] = ko.observable(NaN)
    loadContext kvm, args

    kvm['caseInfo'] ?= ""
    ko.applyBindings kvm, $('#partnersSearch-content')[0]
    $("#case-info").popover { template: $("#custom-popover").html() }
    q.search()

    # responsive web design damn it, can't use heigth: Nvw because of header
    resizeResults()
    $(window).resize resizeResults

  # key to retrieve data for partnerSearch screen from localstore
  storeKey: storeKey
  # key that service should subscribe to get data back
  subName: subName
  open: open
  template: tpl
  partials: partialize
    search        : srch
    city          : city
    make          : make
    dealer        : dlr
    mobilePartner : mbp
    workNow       : wn
    services      : srvs
    priority2     : pr2
    priority3     : pr3