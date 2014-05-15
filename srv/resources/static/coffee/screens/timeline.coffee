define ["text!tpl/screens/timeline.html"
      , "d3"
      , "model/main"
      , "components/table"]
    , (tpl, d3, Main, Table) ->

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
        spairs = _.zip states, states[1..-1].concat [{ ctime: Infinity }]
        # `drawBegin` and `drawEnd` is dates that really will be drawn,
        # and they always in the selection range. When real states borders are
        # outside of the selection range then `draw(Begin|End)` will stick to
        # the selection boundaries.
        # Server will always send more data than selected, so we can build rects
        # near borders.
        srects = _.map spairs, ([s1, s2]) ->
          id:    s1.id
          state: s1.state
          begin: s1.ctime
          end:   s2.ctime
          drawBegin: s1.ctime
          drawEnd:   s2.ctime
        srects = _.filter srects, (s) =>
          s.begin <= @endDate and s.end >= @startDate
        if _.first(srects).begin <= @startDate
          _.first(srects).drawBegin = @startDate
        _.last(srects).drawEnd  = @endDate
      @data = d3.entries(srects)
      if @chart then @draw()

    showRangePicker: (element) =>
      moment().lang("ru")
      $picker = $(element).find(".rangepicker")
      cb = (start, end) =>
        @startDate = start.toDate()
        @endDate = end.toDate()
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

    rectWidth: (scale, rect) =>
      scale(rect.value.drawEnd) - scale(rect.value.drawBegin)

    drawTimeLine: (sel, scale, margin, height) ->
      rs = sel.selectAll(".bar").data(@data, (d) -> d.value.id)
        .attr("x",     (d) => scale(d.value.drawBegin))
        .attr("width", (d) => @rectWidth(scale, d))
      rs.enter().append("rect")
        .attr("class", (d) -> "#{d.value.state} bar")
        .attr("x",     (d) => scale(d.value.drawBegin))
        .attr("y", "#{margin}")
        .attr("height", height)
        .attr("width", (d) => @rectWidth(scale, d))
      rs.exit().remove()

    draw: =>
      domain = [
        d3.min(@data, (d) -> d.value.drawBegin),
        d3.max(@data, (d) -> d.value.drawEnd)]
      @xMainScale.domain(domain)
      @xMiniScale.domain(domain)

      @main.select(".x.main.axis").call(@xMainAxis)
      @mini.select(".x.mini.axis").call(@xMiniAxis)

      # draw large scale chart
      @drawTimeLine(@itemRects, @xMainScale, @margin, 100)

      # draw mini chart
      @drawTimeLine(@mini, @xMiniScale, -@margin, 5)

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
        (d.value.drawBegin < domain[1].getTime()) or
        (d.value.drawEnd > domain[0].getTime())

      ###
        replace showing data on the large scale chart
      ###
      rects = @itemRects.selectAll(".bar")
        .data(visData)
          .attr("x", (d) => @xMainScale(d.value.drawBegin))
          .attr("width", (d) => @rectWidth(@xMainScale, d))

    onClose: (cb) =>
      @closeCbs.push cb

    closeClick: (data) =>
      el(data.elementId).remove()
      _.each @closeCbs, (cb) ->
        cb(data)

  setupScreen = (viewName, args) ->
    columns = ['login', 'realName', 'roles']
    viewModel = 'usermeta'
    dataModel = 'Usermeta'
    table = new Table {viewModel, dataModel, columns}

    timelines = ko.observableArray()

    table.onClick (user) ->
      unless _.find(timelines(), (t) -> t.user.id is user.id)
        $("html, body").animate({ scrollTop: $(document).height() }, "slow")
        timeline = new Timeline({user: user})
        timeline.onClose -> timelines.remove timeline
        timelines.push timeline

    kvm = {table, timelines}
    ko.applyBindings(kvm, el(viewName))

  constructor: setupScreen
  template: tpl
