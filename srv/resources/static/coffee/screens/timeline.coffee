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
      @legend_data = [ {text: "Залогинен", type: "login"}
              , {text: "Готов" , type: "ready"}
              , {text: "Занят", type: "busy"}
              , {text: "Обед", type: "lunch"}
              , {text: "Перерыв", type: "break"}
              , {text: "Служебный перерыв", type: "service_break"}
              , {text: "Разлогинен", type: "logout"} ]

    setData: (data) =>
      @data = d3.entries(data)
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
            .attr("class", (d) -> d.type)
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
      @xMainScale = d3.time.scale()
        .range([0, @width])

      # custom time format
      # different from the default format by:
      # 1 PM => 13:00
      format = d3.time.format.multi([
        [".%L", (d) -> d.getMilliseconds()],
        [":%S", (d) -> return d.getSeconds()],
        ["%H:%M", (d) -> return d.getMinutes()],
        ["%H:%M", (d) -> return d.getHours()],
        ["%a %d", (d) -> return d.getDay() && d.getDate() != 1],
        ["%b %d", (d) -> return d.getDate() != 1],
        ["%B", (d) -> return d.getMonth()],
        ["%Y", () -> true]
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

      # provides an ability to select a part of mini chart
      @brush = d3.svg.brush()
        .x(@xMiniScale)
        .on("brush", @brushed)

      @draw()

    draw: =>
      @xMainScale.domain(d3.extent(@data, (d) -> new Date(d.value.timestamp)))
      @xMiniScale.domain([d3.min(@data, (d) -> new Date(d.value.timestamp)), new Date()])

      # draw large scale chart
      @itemRects.selectAll("rect")
        .data(@data)
        .enter()
        .append("rect")
          .attr("class", (d) -> d.value.type)
          .attr("x", (d) => @xMainScale(new Date(d.value.timestamp)))
          .attr("y", "#{@margin}")
          .attr("height", 100)
          .attr("width", (d) => @xMainScale(@nextEventDate(d)) - @xMainScale(new Date(d.value.timestamp)))

      # draw Axis on large scale chart
      @main.append("g")
        .attr("class", "x main axis")
        .call(@xMainAxis)

      # draw mini chart
      @mini.selectAll("rect")
        .data(@data)
        .enter()
        .append("rect")
          .attr("class", (d) -> d.value.type)
          .attr("x", (d) => @xMiniScale(new Date(d.value.timestamp)))
          .attr("y", -@margin)
          .attr("height", 5)
          .attr("width", (d) => @xMiniScale(@nextEventDate(d)) - @xMiniScale(new Date(d.value.timestamp)))

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
        begin = d.value.timestamp
        end = @nextEventDate(d).getTime()
        (begin < domain[1].getTime()) or (end > domain[0].getTime())

      ###
        replace showing data on the large scale chart
      ###
      rects = @itemRects.selectAll("rect")
        .data(visData)
          .attr("x", (d) => @xMainScale(new Date(d.value.timestamp)))
          .attr("width", (d) => @xMainScale(@nextEventDate(d)) - @xMainScale(new Date(d.value.timestamp)))

      rects.enter()
        .append("rect")
          .attr("class", (d) -> d.value.type)
          .attr("x", (d) => @xMainScale(new Date(d.value.timestamp)))
          .attr("y", "#{@margin}")
          .attr("height", 100)
          .attr("width", (d) => @xMainScale(@nextEventDate(d)) - @xMainScale(new Date(d.value.timestamp)))

      rects.exit().remove()

    nextEventDate: (d) =>
      next = parseInt(d.key) + 1
      finded = _.find @data, (d) -> d.key is "#{next}"
      if finded
        new Date(finded.value.timestamp)
      else
        new Date()

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
          # FIXME: replace this fake data
          data = [ {timestamp: 1396479191554, type: "login"}
                 , {timestamp: 1396579292554, type: "logout"}
                 , {timestamp: 1396679393554, type: "login"}
                 , {timestamp: 1396779393554, type: "ready"}
                 , {timestamp: 1396879393554, type: "busy"}
                 , {timestamp: 1396979393554, type: "lunch"}
                 , {timestamp: 1397079494554, type: "break"}
                 , {timestamp: 1397179595554, type: "service_break"}
                 , {timestamp: 1397279696554, type: "logout"}
                 , {timestamp: 1397379797554, type: "login"}
                 , {timestamp: 1397479898554, type: "logout"} ]
          timeline = new Timeline({user: user})
          timeline.setData(data)
          kvm.timelines.push(timeline))
    Screenman.showScreen viewName

    ko.applyBindings(kvm, el(viewName))

  constructor: setupScreen
  template: tpl
