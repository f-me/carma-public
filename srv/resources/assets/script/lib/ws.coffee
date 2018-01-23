{_} = require "carma/vendor"

# Just as WebSocket but will try to reconnect whith frequency, specified
# by `@freq` in ms

module.exports.WS = class WS
  constructor: (@url, @reopen = true, @freq = 10000) -> @q = []; @open()

  onopen:    null
  onclose:   null
  onerror:   null
  onmessage: null

  open: =>
    @ws = new WebSocket(@url)

    @ws.onclose = (v) =>
      setTimeout((=> @open()), @freq) if @reopen
      @onclose(v) if _.isFunction(@onclose)

    @ws.onopen = (v) =>
      @onopen(v)  if _.isFunction(@onopen)
      @send.apply @q.shift() until _.isEmpty @q

    @ws.onerror = (v) =>
      @onerror(v) if _.isFunction(@onerror)

    @ws.onmessage =  (v) => @onmessage(v) if _.isFunction(@onmessage)

  close: =>
    @reopen = false
    @ws.close()

  send: (args...) =>
    if @ws.readyState == @ws.OPEN
      @ws.send(args)
    else
      @q.push(args)
