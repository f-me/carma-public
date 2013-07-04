define [ "utils"
       , "model/main"
       , "text!tpl/screens/partnersSearch.html"
       ], (utils, m, tpl) ->

  cbMeta = (l, n) ->
    name: n
    meta: { label: l }

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
  srch = Mustache.render txt, fh['search'] #{ meta: { label: "Поиск" } }
  city = Mustache.render md,  fh['city'] #{ meta: { label: "Город" } }
  make = Mustache.render md,  fh['make'] #{ meta: { label: "Марка" } }
  srvs = Mustache.render md,  fh['services'] #{ meta: { label: "Марка" } }
  pr2  = Mustache.render md,  fh['priority2'] #{ meta: { label: "ПБГ"   } }
  pr3  = Mustache.render md,  fh['priority3'] #{ meta: { label: "ПНГ"   } }
  dlr  = Mustache.render cb,  fh['isDealer'] # cbMeta("Дилер", "dealer")
  mbp  = Mustache.render cb,  fh['mobilePartner'] #cbMeta("Мобильный партнер", "mobilePartner")
  wn   = Mustache.render cb,  fh['workNow'] #cbMeta("Работают сейчас", "workNow")

  constructor: ->
    kvm = m.buildKVM(model, "partnersSearch-content")
    ko.applyBindings kvm, $('#partnersSearch-content')[0]
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