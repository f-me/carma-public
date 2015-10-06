define ["utils", "screens/rkc.jade"],
  (utils, tpl) ->
    weatherCityDict =
      utils.newModelDict "City", true, {dictionaryKey: "value"}
    rkcFillWeather = (result, cities) ->
      cities.removeAll()
      for r in result.weather
        cities.push
          city: r.city
          cityname: weatherCityDict.getLab r.city
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
        # Fill programs
        programs = for v in (utils.newModelDict "Program", true).source
          p =
            id: v.value
            name: v.label

        programs.unshift { id: "*", name: "Все" }

        ko.applyBindings(programs, el("program-select"))

        # Fill cities
        cities = for v in utils.newModelDict("City").source
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
        mt = utils.mkDataTable mobit, { bFilter: true, bInfo: false }

        # Fill general info
        totalServices = $('#total-services')
        averageStart = $('#average-towage-tech-start')
        procAvgTime = $('#processing-average-time')
        assignAvgTime = $('#assignAvgTime')
        realprocAvgTime = $('#realprocAvgTime')
        calculated = $('#calculated-cost')
        mechanic = $('#mechanic')
        averageEnd = $('#average-towage-tech-end')
        limited = $('#limited-cost')

        satisfied = $('#satisfied-percentage')

        totalActions = $('#total-actions')
        totalIncompleteActions = $('#total-incomplete-actions')

        ko.applyBindings(weatherCityDict.source,
                el "rkc-weather-city-select")

        cityDict = utils.newModelDict("City")
        actDict = utils.newModelDict "ActionType"
        srvDict = utils.newModelDict "ServiceType"

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
            from = Date.parseExact($('#rkc-date-from').val(), "dd.MM.yyyy")
            to   = Date.parseExact($('#rkc-date-to').val(), "dd.MM.yyyy")

            ct.fnClearTable()
            bt.fnClearTable()

            # Update general statistics fields from JSON response data
            totalServices.val(result.case.summary.total)
            averageStart.val(utils.formatSecToMin(result.stats.towStartAvgTime))
            procAvgTime.val(utils.formatSecToMin(result.stats.procAvgTime))
            assignAvgTime.val(utils.formatSecToMin(result.stats.assignAvgTime))
            realprocAvgTime.val(utils.formatSecToMin(result.stats.realprocAvgTime))
            calculated.val(result.case.summary.calculated)
            mechanic.val(result.case.summary.mech)
            averageEnd.val(utils.formatSecToMin(result.case.summary.duration))
            limited.val(result.case.summary.limited)

            satisfied.val(result.case.summary.satisfied)

            # Update services table
            crows = for cinfo in result.case.services
              crow = [
                srvDict.getLab(cinfo.name),
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
                actDict.getLab(binfo.name),
                binfo.total,
                binfo.undone,
                fmttime(binfo.average)]

            bt.fnAddData(brows)

            # Fill mobile partners table
            $.getJSON "/_/Partner/?isMobile=true&isActive=true",
              (result) ->
                mt.fnClearTable()
                mrows = for minfo in result
                  mrow =
                    [ minfo.name
                    , if minfo.mtime.length > 0
                        new Date(minfo.mtime).toString('dd.MM.yyyy HH:mm')
                      else
                        ""
                    , cityDict.getLab(minfo.city) || ''
                    , if minfo.addrs.length > 0
                        (utils.getKeyedJsonValue minfo.addrs, "fact") || ""
                      else
                        ""
                    ]
                # this will susppress datables alerts about missing rows #2415
                return if _.isEmpty(mrows)
                mt.fnAddData(mrows)

            # Update complaints
            complaints.removeAll()
            for comp in result.complaints
              complaints.push({
                caseid: comp.caseid,
                url: "/#case/" + comp.caseid,
                services: for s in comp.services
                  srvname = srvDict.getLab s
              }))

        partners = ko.observableArray([])
        initRKCDate update, partners
        fillRKCFilters update, partners

        global.rkcData = {}

        update()
        updatePartners(partners)
        updateWeather()


    # function which return object with functions returning functions


    { constructor : setupRKCScreen
    , template    : tpl()
    , initRKCDate          : initRKCDate
    , fillRKCFilters       : fillRKCFilters
    , rkcWeatherRemoveCity : rkcWeatherRemoveCity
    , rkcWeatherAddCity    : rkcWeatherAddCity
    , updateWeather        : updateWeather
    , updatePartners       : updatePartners
    }
