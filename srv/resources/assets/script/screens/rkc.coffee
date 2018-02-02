{$, _, ko} = require "carma/vendor"
utils = require "carma/utils"
template = require "carma-tpl/screens/rkc.pug"

weatherCityDict = utils.newModelDict "City", true, dictionaryKey: "value"

rkcFillWeather = (result, wcities) ->
  do wcities.removeAll
  for r in result.weather
    wcities.push
      city     : r.city
      cityname : weatherCityDict.getLab r.city
      temp     : r.temp
      delCity  : rkcWeatherRemoveCity wcities, r.city
  wcities.sort (l, r) -> l.cityname > r.cityname

getCookie = (key) ->
  x = document.cookie.match new RegExp "#{key}=([^;]*)"
  x && JSON.parse unescape x[1]

updateCookie = (key, def, fn) ->
  obj = getCookie(key) || def
  val = escape JSON.stringify fn obj
  document.cookie = "#{key}=#{val}; path=/;"

rkcWeatherRemoveCity = (wcities, name) -> () ->
  updateCookie 'rkcWeather', [], (val) ->
    _.filter val, (c) -> c != name
  updateWeather wcities

rkcWeatherAddCity = (wcities, name) ->
  updateCookie 'rkcWeather', [], (val) ->
    val.push name unless _.contains val, name
    val
  updateWeather wcities

updateWeather = (wcities) ->
  cfg = document.cookie.match /rkcWeather=([^;]*)/
  $.getJSON "/rkc/weather?cities=#{cfg?[1] || ''}",
            (result) -> rkcFillWeather result, wcities

updatePartners = ({partners}, dateFilterKVM) ->
  from = dateFilterKVM.dateFrom()
  to   = dateFilterKVM.dateTo()

  partnerArgs = "?" + ["from=#{from || ""}", "to=#{to || ""}"].join "&"

  $.getJSON "/rkc/partners#{partnerArgs}", (result) ->
    do partners.removeAll
    partners.push {id: "*", name: "Все"}
    partners.push {id: r,   name: r    } for r in result

getInitRKCDate = () ->
  d1 = new Date
  d2 = new Date
  d2.setDate d1.getDate() + 1

  return
    from : d1.toString 'dd.MM.yyyy'
    to   : d2.toString 'dd.MM.yyyy'

buildRKCFiltersKVM = ->
  cities = ko.observableArray \
    (id: v.value, name: v.label \
      for v in utils.newModelDict("City").source)

  cities.unshift {id: "*", name: "Все"}

  programs = ko.observableArray \
    (id: v.value, name: v.label \
      for v in utils.newModelDict("Program", true).source)

  programs.unshift {id: "*", name: "Все"}

  partners = ko.observableArray [{id: "*", name: "Все"}]

  kvm = {
    cities
    programs
    partners

    selectedCity    : ko.observable null
    selectedProgram : ko.observable null
    selectedPartner : ko.observable null

    optionsText  : ({name}) -> name
    optionsValue : ({id})   -> id
  }

  kvm

getFilterRKCArgs = (filtersKVM, dateFilterKVM) ->
  city    = filtersKVM.selectedCity()
  prog    = filtersKVM.selectedProgram()
  partner = filtersKVM.selectedPartner()
  from    = dateFilterKVM.dateFrom()
  to      = dateFilterKVM.dateTo()

  args = ["from=#{from || ''}", "to=#{to || ''}"]
  args.push "program=#{prog}"    if prog    isnt "*"
  args.push "city=#{city}"       if city    isnt "*"
  args.push "partner=#{partner}" if partner isnt "*"
  "?#{args.join "&"}"

fillRKCMobilePartners = (mt, cityDict) ->
  # Fill mobile partners table
  $.getJSON "/_/Partner/?isMobile=true&isActive=true", (result) ->
    do mt.fnClearTable

    mrows = for minfo in result then [
      minfo.name

      if minfo.mtime.length > 0
        new Date(minfo.mtime).toString 'dd.MM.yyyy HH:mm'
      else
        ""

      cityDict.getLab(minfo.city) || ''

      if minfo.addrs.length > 0
        utils.getKeyedJsonValue(minfo.addrs, "fact") || ""
      else
        ""
    ]

    # this will susppress datables alerts about missing rows #2415
    mt.fnAddData mrows unless _.isEmpty mrows

setupRKCScreen = (viewName, args) ->
  caset    = $("#rkc-services-table")
  actionst = $("#rkc-actions-table")
  weathert = $('#rkc-weather-table')
  complt   = $('#rkc-complaints-table')
  mobit    = $('#rkc-mobile-partners-table')

  isInitialized = (x) -> x.hasClass "dataTable"
  return if [caset, actionst, weathert, complt, mobit].some isInitialized

  wcities = ko.observableArray []

  dateFilterKVM = do ->
    x = getInitRKCDate()
    dateFrom : ko.observable x.from
    dateTo   : ko.observable x.to

  ko.applyBindings wcities,       document.getElementById "rkc-weather-table"
  ko.applyBindings dateFilterKVM, document.getElementById "rkc-date-from"
  ko.applyBindings dateFilterKVM, document.getElementById "rkc-date-to"

  ct = utils.mkDataTable caset,    {bFilter: false, bInfo: false}
  bt = utils.mkDataTable actionst, {bFilter: false, bInfo: false}
  mt = utils.mkDataTable mobit,    {bFilter: true,  bInfo: false}

  # Fill general info
  totalServices          = $('#total-services')
  averageStart           = $('#average-towage-tech-start')
  procAvgTime            = $('#processing-average-time')
  assignAvgTime          = $('#assignAvgTime')
  realprocAvgTime        = $('#realprocAvgTime')
  calculated             = $('#calculated-cost')
  mechanic               = $('#mechanic')
  averageEnd             = $('#average-towage-tech-end')
  limited                = $('#limited-cost')

  satisfied              = $('#satisfied-percentage')

  totalActions           = $('#total-actions')
  totalIncompleteActions = $('#total-incomplete-actions')

  do ->
    selectedWeatherCity = ko.observable null

    kvm = {
      selectedWeatherCity
      weatherCities: weatherCityDict.source
      addWeather: -> rkcWeatherAddCity wcities, selectedWeatherCity()

      optionsText  : ({label}) -> label
      optionsValue : ({value}) -> value
    }

    ko.applyBindings kvm, document.getElementById "rkc-weather-form"

  cityDict = utils.newModelDict "City"
  actDict  = utils.newModelDict "ActionType"
  srvDict  = utils.newModelDict "ServiceType"

  # Complaints
  complaints = ko.observableArray []
  ko.applyBindings complaints, document.getElementById "rkc-complaints-table"

  fmttime = (tm) ->
    fmt = (x) -> if x < 10 then "0#{x}" else "#{x}"
    "#{Math.floor tm / 60}:#{fmt tm % 60}"

  filtersKVM = buildRKCFiltersKVM()
  ko.applyBindings filtersKVM, document.getElementById "rkc-filters"

  update = () ->
    args = getFilterRKCArgs filtersKVM, dateFilterKVM

    $.getJSON "/rkc#{args}", (result) ->
      from = Date.parseExact dateFilterKVM.dateFrom(), "dd.MM.yyyy"
      to   = Date.parseExact dateFilterKVM.dateTo(),   "dd.MM.yyyy"

      do ct.fnClearTable
      do bt.fnClearTable

      # Update general statistics fields from JSON response data
      totalServices.val   result.case.summary.total
      averageStart.val    utils.formatSecToMin result.stats.towStartAvgTime
      procAvgTime.val     utils.formatSecToMin result.stats.procAvgTime
      assignAvgTime.val   utils.formatSecToMin result.stats.assignAvgTime
      realprocAvgTime.val utils.formatSecToMin result.stats.realprocAvgTime
      calculated.val      result.case.summary.calculated
      mechanic.val        result.case.summary.mech
      averageEnd.val      utils.formatSecToMin result.case.summary.duration
      limited.val         result.case.summary.limited

      satisfied.val result.case.summary.satisfied

      # Update services table
      ct.fnAddData ([
        srvDict.getLab cinfo.name
        cinfo.total
        utils.formatSecToMin cinfo.delay
        utils.formatSecToMin cinfo.duration
        cinfo.calculated
        cinfo.limited
      ] for cinfo in result.case.services)

      totalActions.val           result.back.summary.total
      totalIncompleteActions.val result.back.summary.undone

      # Update actions table
      bt.fnAddData ([
        actDict.getLab binfo.name
        binfo.total
        binfo.undone
        fmttime binfo.average
      ] for binfo in result.back.actions)

      fillRKCMobilePartners mt, cityDict

      # Update complaints
      do complaints.removeAll
      for comp in result.complaints
        complaints.push
          caseid   : comp.caseid
          url      : "/#case/#{comp.caseid}"
          services : (srvDict.getLab s for s in comp.services)

  do update
  updateWeather wcities
  ko.applyBindings {update}, document.getElementById "rkc-reload"

  for x in [dateFilterKVM.dateFrom, dateFilterKVM.dateTo]
    x.subscribe -> updatePartners filtersKVM, dateFilterKVM

  # first initial fetch (initial dates is already set)
  updatePartners filtersKVM, dateFilterKVM

  x.subscribe update for x in [
    filtersKVM.selectedCity
    filtersKVM.selectedProgram
    filtersKVM.selectedPartner
  ]


module.exports = {
  constructor: setupRKCScreen
  template
}
