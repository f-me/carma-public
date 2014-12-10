define ["lib/ws"], (WS) ->

  if window.location.protocol == "https:"
    messengerUrl = "wss://#{location.hostname}:#{location.port}/wsmessenger"
  else
    messengerUrl = "ws://#{location.hostname}:#{location.port}/wsmessenger"

  subscribe = (ws, topic) => ws.send JSON.stringify subscribe: topic

  multisubKVM: (kvms) =>
    ws = new WS(messengerUrl)
    cachekv = {}
    ws.onopen = => for k in ko.utils.unwrapObservable(kvms) when k.id()
      topic = "#{k._meta.model.name}:#{k.id()}"
      cachekv[topic] = k
      subscribe ws, topic
    ws.onmessage = (ev) =>
      data = JSON.parse ev.data
      cachekv[data.topic]?._meta?.q?.saveSuccessCb?(_.identity)(data.payload)
    ws.onclose = =>
      cachekv = {}
    return ws

  subscribe: (topic, cb) =>
    ws = new WS(messengerUrl)
    ws.onopen  = => subscribe(ws, topic)
    ws.onmessage = (ev) => cb((JSON.parse ev.data).payload)
    return ws
