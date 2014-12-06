define ["utils", "text!tpl/screens/rkc.html"],
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

    # Mini #rkc only shows mobile partners
    setupRKCScreen = (viewName, args) ->
      setTimeout ->
        mobit = $('#rkc-mobile-partners-table')

        mt = utils.mkDataTable mobit, { bFilter: true, bInfo: false }

        cityDict = utils.newModelDict("City")

        update = () ->
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

        update()


    # function which return object with functions returning functions


    { constructor : setupRKCScreen
    , template    : tpl
    , initRKCDate          : initRKCDate
    , fillRKCFilters       : fillRKCFilters
    , rkcWeatherRemoveCity : rkcWeatherRemoveCity
    , rkcWeatherAddCity    : rkcWeatherAddCity
    , updateWeather        : updateWeather
    , updatePartners       : updatePartners
    }
