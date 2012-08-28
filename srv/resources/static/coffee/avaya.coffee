
this.setupAvayaWidget = (domId,ext,pwd) ->
  url = "ws://#{location.hostname}:8001/avaya/#{ext}/#{pwd}"
  ws = new WebSocket(url)
  panel = $(domId)
  ws.onopen = ->
    panel.show()
    panel.find('#avaya-accept').click ->
      ws.send('acceptCall')
  ws.onclose = ->
    panel.hide()
  ws.onerror = ->
    panel.hide()
  ws.onmessage = (ev) ->
    msg = JSON.parse(ev.data)
    if msg.type == "ringer"
      if msg.ringerMode == "ringing"
        panel.addClass("open")
      else
        panel.removeClass("open")
    else if msg.type == "display"    
      panel.find("#avaya-info").text(msg.display)
    
    
