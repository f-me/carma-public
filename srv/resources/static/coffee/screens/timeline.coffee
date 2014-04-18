define ["text!tpl/screens/timeline.html"
      , "d3"
      , "screenman"]
    , (tpl, d3, Screenman) ->

  class Timeline

    constructor: (bind) ->
      @margin = 25

      @width = 720
      @height = 240

      @user = bind.user
      @elementId = "chart-#{@user.id}"
      @title = "Таймлайн - #{@user.realName}"
      @legend_data = [ {text: "Готов" ,     state: "Ready"     }
                     , {text: "Занят",      state: "Busy"      }
                     , {text: "Разлогинен", state: "LoggedOut" }
                     ]

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

    showTimeline: (element) =>
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

      @xMiniScale = d3.time.scale()
        .range([0, @width])

      @xMiniAxis = d3.svg.axis()
        .scale(@xMiniScale)
        .orient("bottom")
        .tickFormat(format)

      # provides an ability to select a part of mini chart
      @brush = d3.svg.brush()
        .x(@xMiniScale)
        .on("brush", @brushed)

      @draw()

    rectWidth: (scale, rect) => scale(rect.value.end) - scale(rect.value.begin)

    drawRect: (scale, margin, h) => (sel) =>
      sel.append("rect")
        .attr("class", (d) -> d.value.state)
        .attr("x",     (d) => scale(d.value.begin))
        .attr("y", "#{margin}")
        .attr("height", h)
        .attr("width", (d) => @rectWidth(scale, d))

    draw: =>
      @xMainScale.domain(d3.extent(@data, (d) -> d.value.begin))
      @xMiniScale.domain([d3.min(@data, (d) -> d.value.begin), new Date()])

      # draw large scale chart
      @itemRects.selectAll("rect")
        .data(@data)
        .enter()
        .call(@drawRect(@xMainScale, @margin, 100))

      # draw Axis on large scale chart
      @main.append("g")
        .attr("class", "x main axis")
        .call(@xMainAxis)

      # draw mini chart
      @mini.selectAll("rect")
        .data(@data)
        .enter()
        .call(@drawRect(@xMiniScale, -@margin, 5))

      # draw Axis on mini chart
      @mini.append("g")
        .attr("transform", => "translate(0, #{@margin / 2})")
        .attr("class", "x mini axis")
        .call(@xMiniAxis)

      # append component for select the part of chart to mini chart
      @mini.append("g")
        .attr("class", "brush")
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

  setupScreen = (viewName, args) ->
    kvm =
      timelines: ko.observableArray()

    tableParams =
      tableName: "users"
      objURL: "/_/Usermeta"

    users = []
    objsToRows = (objs) ->
      users = objs
      rows = for obj in objs
        [obj.id
        ,"#{obj.realName}" || ''
        ]

    table = Screenman.addScreen(viewName, -> )
      .addTable(tableParams)
      .setObjsToRowsConverter(objsToRows)
      .on("click.datatable", "tr", ->
        i = table.dataTable.fnGetPosition(@)
        user = users[i]
        unless _.find(kvm.timelines(), (t) -> t.user.id is user.id)
          $("html, body").animate({ scrollTop: $(document).height() }, "slow")
          $.getJSON "/userStates/#{user.id}", (states) ->
            timeline = new Timeline({user: user})
            timeline.setData(states)
            kvm.timelines.push(timeline))

    Screenman.showScreen viewName

    ko.applyBindings(kvm, el(viewName))

  constructor: setupScreen
  template: tpl
