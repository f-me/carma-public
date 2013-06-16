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
          partners.push({ id: "", name: "Все" })
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

        updateps = () -> updatePartners(partners)

        dateFrom.change ->
          updateps()
          updater()
        dateTo.change ->
          updateps()
          updater()

    fillRKCFilters = (updater, partners) ->
      setTimeout ->
        dict = global.dictValueCache

        # Fill programs
        programs = for v in global.dictionaries.Programs.entries
          p =
            id: v.value
            name: v.label

        programs.unshift { id: "", name: "Все" }

        ko.applyBindings(programs, el("program-select"))

        # Fill cities
        cities = for v in global.dictionaries.DealerCities.entries
          c =
            id: v.value
            name: v.label

        cities.unshift { id: "", name: "Все" }

        ko.applyBindings(cities, el("city-select"))

        # Fill partner
        partners.push({ id: "", name: "Все" })

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

      args = "?" + ["program=" + prog, "city=" + city, "partner=" + partner, "from=" + from, "to=" + to].filter((x) -> x).join("&")

      return args

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

            $.getJSON "/all/partner/?select=isMobile==1,isActive==1" +
                      "&fields=name,mtime,city,addrDeFacto",
              (result) ->
                mt.fnClearTable()
                mrows = for minfo in result
                  mrow =
                    [ minfo.name
                    , if minfo.mtime.length > 0
                        new Date(JSON.parse minfo.mtime).
                        toString('dd.MM.yyyy HH:mm')
                      else
                        ""
                    , minfo.addrDeFacto
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
        global.rkcData.updateHandler = setInterval(update, 30000)

        updateSMS()
        update()
        updatePartners(partners)
        updateWeather()

    removeRKCScreen = ->
        h = global.rkcData.smsHandler
        clearInterval h if h?
        t = global.rkcData.updateHandler
        clearInterval t if t?

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
