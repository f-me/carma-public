define [], ->
  parseWorkTimes = (str) ->
    _.compact _.map str.split(','), parseWorkTime

  parseWorkTime = (str) ->
    s =
      str.match /(\d{2}:\d{2})\s*-\s*(\d{2}:\d{2})\s*\/((\d{1}-\d{1})|(\d{1}))/
    return if !s or s.length < 6
    [x, beg, end, days] = s
    if days.length > 1
      # when days represented as interval
      ds = _.map days.split('-'), (v) -> parseInt(v)
      return unless ds[0] > 0 and ds[1] < 8 and ds[0] < ds[1]
      days = [ds[0] .. ds[1]]
    else
      days = [parseInt(days)]
    # check that begin and end time is correct
    return unless _.all [beg, end], (v) -> Date.parseExact(v, "HH:mm")
    { begin: beg, end: end, days: days }

  isWorkingNow = (wtimes) ->
    now = Date.now().toString("HH:mm")
    day = Date.now().getDay()
    # special case for sunday
    day ||= 7
    _.any wtimes, ({begin, end, days}) ->
      begin <= now and end >= now and _.contains days, day

  isWorkingNow: isWorkingNow
  parseWorkTimes: parseWorkTimes