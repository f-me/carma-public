define [], () ->
  x = 1
  data = _.map([1..36], -> x *= 0.892)
  int = null

  start: ->
    svg = document.getElementById('spiral')
    box = svg.getBoundingClientRect()

    squares = d3.select("#spiral")
      .selectAll("g")
      .data(data)
      .enter()
        .append("svg:g")
        .classed("square", true)

    squares.append("svg:rect")
      .attr("rx", 1)
      .attr("ry", 1)
      .attr("x", -2)
      .attr("y", -2)
      .attr("width",  4)
      .attr("height", 4)
      .attr("transform", (d) -> "scale(#{d*box.width})")
      .style("fill", (d,i) -> ["#dadadb", "#dddddd"][i%2])

    t = 0
    int = setInterval(
      ->
        t = if t >= 1200 then 0 else t + 1
        alpha = 5*360*t/1200
        squares
          .attr("transform", (d, i) ->
            cx = box.width/2 + 36*Math.cos(-alpha/5)
            cy = box.height/2 + 36*Math.sin(-alpha/5)
            "translate(#{[cx, cy]})rotate(#{alpha*i})")
      , 110)

  stop: -> clearInterval int
