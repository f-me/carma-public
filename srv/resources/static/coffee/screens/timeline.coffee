define ["text!tpl/screens/timeline.html"
      , "d3"
      , "utils"]
    , (tpl, d3, Utils) ->


  class Table
    constructor: (opt) ->
      @limitDef = 10
      @offsetDef = 0

      @columns = opt.columns
      @data = opt.data

      @items = ko.observableArray(_.clone @data)

      @limit = ko.observable(@limitDef)
      @offset = ko.observable(@offsetDef)

      @typeahead = ko.observable()
      @typeahead.subscribe (value) =>
        @resetPager()
        @items.removeAll()
        if value
          items = _.filter @data, (row) =>
            _.some @columns, (column) =>
              row[column.name].toLowerCase().indexOf(value.toLowerCase()) isnt -1
          @items(items)
        else
          @items(_.clone @data)

      @prev = ko.computed =>
        offset = @offset() - @limit()
        if offset < 0 then null else offset / @limit() + 1

      @next = ko.computed =>
        length = @items().length
        offset = @offset() + @limit()
        if (length - offset) > 0 then offset / @limit() + 1 else null

      @page = ko.computed =>
        @offset() / @limit() + 1

      @clickCb = []

      @rows = ko.computed =>
        @items.slice(@offset(), @offset() + @limit())

    resetPager: =>
        @limit(@limitDef)
        @offset(@offsetDef)

    prevPage: =>
      @offset(@offset() - @limit())

    nextPage: =>
      @offset(@offset() + @limit())

    onClick: (cb) =>
      @clickCb.push cb

    rowClick: (data) =>
      return =>
        _.each @clickCb, (cb) ->
          cb(data)

    sortColumn: (data) =>
      return =>
        name = data.name
        sort = data.sort
        column = @columns[@columns.indexOf(data)]
        if (sort is "desc") or not sort
          @items.sort (a, b) =>
            @asc(a[name], b[name])
          column.sort = "asc"
        else
          @items.sort (a, b) =>
            @desc(a[name], b[name])
          column.sort = "desc"
        @resetPager()

    asc: (a, b) ->
      if a.toLowerCase() > b.toLowerCase() then 1 else -1

    desc: (a, b) ->
      if a.toLowerCase() < b.toLowerCase() then 1 else -1

  class Timeline

    constructor: (bind) ->
      @margin = 25

      @width = 720
      @height = 240

      @user = bind.user
      @elementId = "chart-#{@user.id}"
      @title = "Таймлайн - #{@user.realName} (#{@user.login})"
      @legend_data = [ {text: "Готов" ,     state: "Ready"     }
                     , {text: "Занят",      state: "Busy"      }
                     , {text: "Разлогинен", state: "LoggedOut" }
                     ]
      @closeCbs = []

    setData: (states) =>
      unless _.isEmpty states
        states = _.map states, (s) -> s.ctime = new Date(s.ctime); s
        spairs = _.zip states, states[1..-1].concat [{ ctime: new Date()}]
        srects = _.map spairs, ([s1, s2]) ->
          id:    s1.id
          state: s1.state
          begin: s1.ctime
          end:   s2.ctime
      @data = d3.entries(srects)
      if @chart then @draw()

    showRangePicker: (element) =>
      moment().lang("ru")
      $picker = $(element).find(".rangepicker")
      cb = (start, end) =>
        s = start.format("YYYY-MM-DD")
        e = end.format("YYYY-MM-DD")
        $.getJSON "/userStates/#{@user.id}/#{s}/#{e}", @setData

      startDate = moment().subtract('days', 1).format('DD MMM, YYYY')
      endDate = moment().format('DD MMM, YYYY')
      $picker.val("#{startDate} - #{endDate}")
      $picker.daterangepicker(
        {
          format: 'DD MMM, YYYY',
          startDate: startDate,
          endDate: endDate,
          locale: {
            applyLabel: 'Установить',
            cancelLabel: 'Отмена',
            fromLabel: 'С',
            toLabel: 'ПО',
            weekLabel: 'Н',
            customRangeLabel: 'Календарь...',
          },
          ranges: {
            'Сегодня': [moment().subtract('days', 1), moment()],
            'Вчера': [ moment().subtract('days', 2)
                     , moment().subtract('days', 1)
                     ],
            'Последние 7 дней': [moment().subtract('days', 6), moment()],
          }
        },
        cb
      )
      # put init data to timeline
      cb(moment().subtract('days', 1), moment())

    showTimeline: (element) =>
      @showRangePicker(element)

      # main container
      @chart = d3.select($(element).find(".chart")[0])
        .append("svg")
        .attr('widht', @width)
        .attr('height', @height)

      # container for rendered large scale chart
      @chart.append("defs").append("clipPath")
        .attr("id", "clip")
        .append("rect")
        .attr("width", @width)
        .attr("height", 100)

      # describes types of data which shown on charts
      @legend = @chart.append("g")
        .attr("class", "chart-legend")
        .attr("transform", => "translate(#{@width + @margin * 2}, #{@margin})")
      @legend.selectAll("g").data(@legend_data)
        .enter()
        .append("g")
        .each((d, i) ->
          g = d3.select(@)
          g.append("rect")
            .attr("class", (d) -> d.state)
            .attr("x", 25)
            .attr("y", i * 25)
            .attr("width", 30)
            .attr("height", 5)
          g.append("text")
            .attr("x", 65)
            .attr("y", i * 25 + 5)
            .attr("width", 40)
            .attr("height", 20)
            .text(d.text)
        )

      # container for large scale chart
      # this chart show only a selected part of timeline
      @main = @chart.append("g")
        .attr("class", "main")
        .attr("transform", => "translate(#{@margin}, 25)")

      # container for bars of which consists large scale chart
      @itemRects = @main.append("g")
        .attr("clip-path", "url(#clip)")

      # container for mini chart
      # shows all @data in his chart
      @mini = @chart.append("g")
        .attr("class", "mini")
        .attr("transform", => "translate(#{@margin}, 200)")
      @mini.append("text")
        .attr("x", @width / 2 - 40)
        .attr("y", 0)
        .attr("width", 40)
        .attr("height", 20)
        .text("(Выберите диапазон)")

      ###
        define scales and axis for charts
        xMain... - for large scale chart
        xMini... - for mini chart
      ###
      @xMainScale = d3.time.scale().range([0, @width])

      # custom time format
      # different from the default format by:
      # 1 PM => 13:00
      format = d3.time.format.multi([
        [".%L", (d)   -> d.getMilliseconds()],
        [":%S", (d)   -> d.getSeconds()],
        ["%H:%M", (d) -> d.getMinutes()],
        ["%H:%M", (d) -> d.getHours()],
        ["%a %d", (d) -> d.getDay() && d.getDate() != 1],
        ["%b %d", (d) -> d.getDate() != 1],
        ["%B", (d)    -> d.getMonth()],
        ["%Y", ()     -> true]
      ])

      @xMainAxis = d3.svg.axis()
        .scale(@xMainScale)
        .orient("top")
        .tickFormat(format)

      # draw Axis on large scale chart
      @main.append("g")
        .attr("class", "x main axis")
        .call(@xMainAxis)

      @xMiniScale = d3.time.scale()
        .range([0, @width])

      @xMiniAxis = d3.svg.axis()
        .scale(@xMiniScale)
        .orient("bottom")
        .tickFormat(format)

      # draw Axis on mini chart
      @mini.append("g")
        .attr("transform", => "translate(0, #{@margin / 2})")
        .attr("class", "x mini axis")
        .call(@xMiniAxis)

      # provides an ability to select a part of mini chart
      @brush = d3.svg.brush()
        .x(@xMiniScale)
        .on("brush", @brushed)

      @mini.append("g")
        .attr("class", "brush")
        .call(@brush)
        .selectAll("rect")
        .attr("y", -@margin * 2)
        .attr("height", @margin * 2)

    rectWidth: (scale, rect) => scale(rect.value.end) - scale(rect.value.begin)

    drawRect: (scale, margin, h) => (sel) =>
      sel.append("rect")
        .attr("class", (d) -> "#{d.value.state} bar")
        .attr("x",     (d) => scale(d.value.begin))
        .attr("y", "#{margin}")
        .attr("height", h)
        .attr("width", (d) => @rectWidth(scale, d))

    draw: =>
      domain = [
        d3.min(@data, (d) -> d.value.begin),
        d3.max(@data, (d) -> d.value.end)]
      @xMainScale.domain(domain)
      @xMiniScale.domain(domain)

      @main.select(".x.main.axis").call(@xMainAxis)
      @mini.select(".x.mini.axis").call(@xMiniAxis)

      # draw large scale chart
      itemRects = @itemRects.selectAll("rect")
        .data(@data)
      itemRects
        .exit().remove()
      itemRects
        .enter()
        .call(@drawRect(@xMainScale, @margin, 100))

      # draw mini chart
      miniRects = @mini.selectAll(".bar")
        .data(@data)
      miniRects
        .exit().remove()
      miniRects
        .enter()
        .call(@drawRect(@xMiniScale, -@margin, 5))

      @mini.select(".brush")
        .call(@brush)
        .selectAll("rect")
        .attr("y", -@margin * 2)
        .attr("height", @margin * 2)

    ###
    #  redraw the large scale chart
    #  when changes selected area on mini chart
    ###
    brushed: =>
      ###
        define wich part of @data to show
      ###
      domain = if @brush.empty() then @xMiniScale.domain() else @brush.extent()
      @xMainScale.domain domain
      @main.select(".x.main.axis").call(@xMainAxis)

      visData = @data.filter (d) =>
        (d.value.begin < domain[1].getTime()) or
        (d.value.end > domain[0].getTime())

      ###
        replace showing data on the large scale chart
      ###
      rects = @itemRects.selectAll("rect")
        .data(visData)
          .attr("x", (d) => @xMainScale(d.value.begin))
          .attr("width", (d) => @rectWidth(@xMainScale, d))

    onClose: (cb) =>
      @closeCbs.push cb

    closeClick: (data) =>
      el(data.elementId).remove()
      _.each @closeCbs, (cb) ->
        cb(data)

  setupScreen = (viewName, args) ->
    model = global.model("usermeta")
    columns = _.map ["login", "realName"], (c) =>
      _.find model.fields, (f) =>
        f.name is c
    data = new Utils.newModelDict("Usermeta")
    table = new Table
      data: data.items
      columns: columns

    kvm =
      timelines: ko.observableArray()
      table: table

    table.onClick (user) ->
      unless _.find(kvm.timelines(), (t) -> t.user.id is user.id)
        $("html, body").animate({ scrollTop: $(document).height() }, "slow")
        timeline = new Timeline({user: user})
        timeline.onClose -> kvm.timelines.remove timeline
        kvm.timelines.push timeline

    ko.applyBindings(kvm, el(viewName))

  constructor: setupScreen
  template: tpl
