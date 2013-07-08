define [ "utils"
       , "text!tpl/screens/partnersLookup.html"
       ], (utils, tpl) ->

  cbMeta = (l, n) ->
    name: n
    meta: { label: l }

  mkPartials = (ps) ->
    $("<script class='partial' id='#{k}'>").html(v)[0].outerHTML for k,v of ps

  partialize = (ps) -> mkPartials(ps).join('')

  txt = $("#text-field-template").html()
  md  = $("#dictionary-many-field-template").html()
  cb  = $("#checkbox-field-template").html()
  srch = Mustache.render txt, { meta: { label: "Поиск" } }
  city = Mustache.render md,  { meta: { label: "Город" } }
  make = Mustache.render md,  { meta: { label: "Марка" } }
  dlr  = Mustache.render cb,  cbMeta("Дилер", "dealer")
  mbp  = Mustache.render cb,  cbMeta("Мобильный партнер", "mobilePartner")
  wn   = Mustache.render cb,  cbMeta("Работают сейчас", "workNow")

  constructor: ->
  template: tpl
  partials: partialize
    search        : srch
    city          : city
    make          : make
    dealer        : dlr
    mobilePartner : mbp
    workNow       : wn