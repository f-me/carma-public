define [ "utils"
       , "model/main"
       , "sync/dipq"
       , "text!tpl/screens/partnersSearch.html"
       ], (utils, m, sync, tpl) ->

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

  fh = {}
  for f in model.fields
    fh[f.name] = f

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

  loadContext = (kvm, args) ->
    return unless args?.model
    return unless localStorage['partnersSearch']
    ctx = JSON.parse localStorage['partnersSearch']
    if args.model == "case"
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
      <li> <b> </b> </li>
      <li> <b> </b> </li>
      <li> <b> </b> </li>
    </ul>
    """
    # cleanup localstore
    localStorage.removeItem 'partnersSearch'

  resizeResults = ->
    t = $("#search-result").offset().top
    w = $(window).height()
    console.log 'resizing', t, w, w-t-50
    $("#search-result").height(w-t-10)

  constructor: (view, args) ->
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
    kvm['selectPartner'] = (partner, ev) -> kvm['selectedPartner'](partner.id)
    loadContext kvm, args

    kvm['caseInfo'] ?= ""
    ko.applyBindings kvm, $('#partnersSearch-content')[0]
    $("#case-info").popover { template: $("#custom-popover").html() }
    q.search()

    # responsive web design damn it, can't use heigth: Nvw because of header
    resizeResults()
    $(window).resize resizeResults

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