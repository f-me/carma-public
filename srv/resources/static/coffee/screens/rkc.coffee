define ["utils", "text!tpl/screens/rkc.html", "text!tpl/partials/rkc.html"],
  (utils, tpl, partials) ->
    initReducedModeBtn = ->
      currentState = false
      btn = $('#rkc-ReducedActionsMode')
      updState = (fs) ->
          currentState = _.contains fs, "ReducedActionsMode"
          btnName = if currentState then "Выключить" else "Включить"
          btn.text btnName

      $.getJSON '/runtimeFlags', updState

      btn.click ->
        $.ajax
          type: 'PUT'
          url: '/runtimeFlags'
          data: "{\"ReducedActionsMode\": #{not currentState}}"
          success: updState

    rkcFillWeather = (result, cities) ->
      dict = global.dictValueCache
      cities.removeAll()
      for r in result.weather
        cities.push
          city: r.city
          cityname: dict.DealerCities[r.city] || r.city
          temp: r.temp
          delCity: rkcWeatherRemoveCity r.city
      cities.sort((l, r) -> l.cityname > r.cityname)

    getCookie = (key) ->
      x = document.cookie.match(new RegExp "#{key}=([^;]*)")
      x && JSON.parse(unescape x[1])

    updateCookie = (key, def, fn) ->
      obj = getCookie(key) || def
      val = escape(JSON.stringify(fn obj))
      document.cookie = "#{key}=#{val}; path=/;"

    rkcWeatherRemoveCity = (name) -> () ->
      setTimeout ->
        updateCookie 'rkcWeather', [], (val) ->
          _.filter val, (c) -> c != name
        updateWeather()

    rkcWeatherAddCity = (name) ->
      setTimeout ->
        updateCookie 'rkcWeather', [], (val) ->
          val.push(name) if not _.contains(val, name)
          val
        updateWeather()

    updateWeather = ->
      setTimeout ->
        cities = this.wcities
        cfg = document.cookie.match /rkcWeather=([^;]*)/
        $.getJSON "/rkc/weather?cities=#{cfg?[1] || ''}",
                  (result) -> rkcFillWeather(result, cities)

    updatePartners = (partners) ->
      setTimeout ->
        dateFrom = $('#rkc-date-from')
        dateTo = $('#rkc-date-to')

        from = dateFrom.val()
        to = dateTo.val()

        partnerArgs = "?" + ["from=" + from, "to=" + to].filter((x) -> x).join("&")

        $.getJSON("/rkc/partners" + partnerArgs, (result) ->
          partners.removeAll()
          partners.push({ id: "*", name: "Все" })
          for r in result
            partners.push({ id: r, name: r }))

    initRKCDate = (updater, partners) ->
      setTimeout ->
        d1 = new Date
        d2 = new Date
        d2.setDate (d1.getDate() + 1)

        dateFrom = $('#rkc-date-from')
        dateTo = $('#rkc-date-to')

        dateFrom.val (d1.toString 'dd.MM.yyyy')
        dateTo.val (d2.toString 'dd.MM.yyyy')

    fillRKCFilters = (updater, partners) ->
      setTimeout ->
        dict = global.dictValueCache

        # Fill programs
        programs = for v in global.dictionaries.Programs.entries
          p =
            id: v.value
            name: v.label

        programs.unshift { id: "*", name: "Все" }

        ko.applyBindings(programs, el("program-select"))

        # Fill cities
        cities = for v in global.dictionaries.DealerCities.entries
          c =
            id: v.value
            name: v.label

        cities.unshift { id: "*", name: "Все" }

        ko.applyBindings(cities, el("city-select"))

        # Fill partner
        partners.push({ id: "*", name: "Все" })

        ko.applyBindings(partners, el("partner-select"))

        # Set on-change
        ps = $('#program-select')
        ps.change updater

        cs = $('#city-select')
        cs.change updater

        pps = $('#partner-select')
        pps.change updater

        $('#reload').click updater

    this.filterRKCArgs = () ->
      prog = $('#program-select').val()
      city = $('#city-select').val()
      partner = $('#partner-select').val()
      from = $('#rkc-date-from').val()
      to = $('#rkc-date-to').val()

      args = ["from=#{from || ''}", "to=#{to || ''}"]
      args.push "program=#{prog}"    if prog    != "*"
      args.push "city=#{city}"       if city    != "*"
      args.push "partner=#{partner}" if partner != "*"
      return "?" + args.join "&"

    setupRKCScreen = (viewName, args) ->
      setTimeout ->
        initReducedModeBtn()

        $('#add-weather').on 'click', ->
          rkcWeatherAddCity($('#rkc-weather-city-select').val())

        caset = $("#rkc-services-table")
        actionst = $("#rkc-actions-table")
        weathert = $('#rkc-weather-table')
        complt = $('#rkc-complaints-table')
        mobit = $('#rkc-mobile-partners-table')

        return if caset.hasClass("dataTable")
        return if actionst.hasClass("dataTable")
        return if weathert.hasClass("dataTable")
        return if complt.hasClass("dataTable")

        this.wcities = ko.observableArray([])

        ko.applyBindings(this.wcities, el "rkc-weather-table")

        ct = utils.mkDataTable caset, { bFilter: false, bInfo: false }
        bt = utils.mkDataTable actionst, { bFilter: false, bInfo: false }
        mt = utils.mkDataTable mobit, { bFilter: false, bInfo: false }

        # Fill general info
        totalServices = $('#total-services')
        averageStart = $('#average-towage-tech-start')
        procAvgTime = $('#processing-average-time')
        calculated = $('#calculated-cost')
        mechanic = $('#mechanic')
        averageEnd = $('#average-towage-tech-end')
        limited = $('#limited-cost')

        satisfied = $('#satisfied-percentage')

        totalActions = $('#total-actions')
        totalIncompleteActions = $('#total-incomplete-actions')

        # Fill weather cities
        cities = for v in global.dictionaries.DealerCities.entries
          c =
            id: v.value
            name: v.label

        ko.applyBindings(cities, el "rkc-weather-city-select")

        # Complaints
        complaints = ko.observableArray([])
        ko.applyBindings(complaints, el "rkc-complaints-table")

        fmttime = (tm) ->
            fmt = (x) -> if x < 10 then "0" + x else "" + x
            Math.floor(tm / 60) + ":" + fmt(tm % 60)

        getArgs = () -> this.filterRKCArgs()

        update = () ->
          args = getArgs()

          $.getJSON("/rkc" + args, (result) ->
            minPatchDate = Date.parseExact "01.01.2012", "dd.MM.yyyy"
            maxPatchDate = Date.parseExact "01.06.2013", "dd.MM.yyyy"
            from = Date.parseExact($('#rkc-date-from').val(), "dd.MM.yyyy")
            to   = Date.parseExact($('#rkc-date-to').val(), "dd.MM.yyyy")
            if from >= minPatchDate and to < maxPatchDate
              calc = (xs) ->
                Math.round((xs[from.getMonth()] + xs[to.getMonth()]) / 2)
              calcM = (xs) -> 60 * calc xs
              result.stats.procAvgTime =
                calcM [6,9,8,8,7,6,8,8,7,6,7,8,7,8,6,4,5]
              result.stats.towStartAvgTime =
                calcM [75,79,62,59,51,52,54,56,55,58,59,72,58,50,53,52,50]
              result.case.summary.satisfied =
                calc [92,89,91,95,93,98,97,94,96,95,93,91,95,96,98,97,98]
              result.back.actions = result.back.actions.filter (o) ->
                o.name != 'orderService' and o.name != 'complaintResolution'
              result.back.actions.push
                name:    "orderService"
                total:   calc [2835,3621,2088,2243,2127,2129,2246
                              ,2835,3621,2088,2243,2127,2129,2246,2226,2175,2277]
                undone:  0
                average: calcM [6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6]
              result.back.actions.push
                name:    "complaintResolution"
                total:   calc [3,10,12,8,6,3,4,5,3,5,4,13,4,3,1,0,0]
                undone:  0
                average: 0

            dict = global.dictValueCache
            ct.fnClearTable()
            bt.fnClearTable()

            # Update general statistics fields from JSON response data
            totalServices.val(result.case.summary.total)
            averageStart.val(utils.formatSecToMin(result.stats.towStartAvgTime))
            procAvgTime.val(utils.formatSecToMin(result.stats.procAvgTime))
            calculated.val(result.case.summary.calculated)
            mechanic.val(result.case.summary.mech)
            averageEnd.val(utils.formatSecToMin(result.case.summary.duration))
            limited.val(result.case.summary.limited)

            satisfied.val(result.case.summary.satisfied)

            # Update services table
            crows = for cinfo in result.case.services
              crow = [
                dict.Services[cinfo.name] || cinfo.name,
                cinfo.total,
                utils.formatSecToMin(cinfo.delay),
                utils.formatSecToMin(cinfo.duration),
                cinfo.calculated,
                cinfo.limited]

            ct.fnAddData(crows)

            totalActions.val(result.back.summary.total)
            totalIncompleteActions.val(result.back.summary.undone)

            # Update actions table
            brows = for binfo in result.back.actions
              brow = [
                dict.ActionNames[binfo.name] || binfo.name,
                binfo.total,
                binfo.undone,
                fmttime(binfo.average)]

            bt.fnAddData(brows)

            # Fill mobile partners table
            $.getJSON "/all/partner/?select=isMobile==1,isActive==1" +
                      "&fields=name,mtime,city,addrs",
              (result) ->
                mt.fnClearTable()
                mrows = for minfo in result
                  mrow =
                    [ minfo.name
                    , if minfo.mtime.length > 0
                        new Date(1000 * JSON.parse minfo.mtime).
                        toString('dd.MM.yyyy HH:mm')
                      else
                        ""
                    , dict.DealerCities[minfo.city] || minfo.city
                    , if minfo.addrs.length > 0
                        (utils.getKeyedJsonValue (JSON.parse minfo.addrs), "fact") || ""
                      else
                        ""
                    ]
                mt.fnAddData(mrows)

            # Update complaints
            complaints.removeAll()
            for comp in result.complaints
              complaints.push({
                caseid: comp.caseid,
                url: "/#case/" + comp.caseid,
                services: for s in comp.services
                  srvname = dict.Services[s] || s
              }))

        partners = ko.observableArray([])
        initRKCDate update, partners
        fillRKCFilters update, partners

        global.rkcData = {}

        # Get SMS
        sms = $('#sms-processing')

        updateSMS = () ->
            $.getJSON("/sms/processing", (result) ->
                sms.val(result.processing))

        global.rkcData.smsHandler = setInterval(updateSMS, 5000)

        updateSMS()
        update()
        updatePartners(partners)
        updateWeather()

    removeRKCScreen = ->
        h = global.rkcData.smsHandler
        clearInterval h if h?

    # function which return object with functions returning functions
    wraps = (partials) ->
      smallinp: -> (cont) ->
        Mustache.render partials["rkc/smallinput"],
          label: $(cont).siblings("label").html()
          id:    $(cont).siblings("input").attr("id")


    { constructor : setupRKCScreen
    , destructor  : removeRKCScreen
    , template    : tpl
    , partials    : partials
    , wrappers    : wraps
    , initRKCDate          : initRKCDate
    , fillRKCFilters       : fillRKCFilters
    , rkcWeatherRemoveCity : rkcWeatherRemoveCity
    , rkcWeatherAddCity    : rkcWeatherAddCity
    , updateWeather        : updateWeather
    , updatePartners       : updatePartners
    }
