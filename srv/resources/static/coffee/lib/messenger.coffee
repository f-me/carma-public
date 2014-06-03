define ["lib/ws"], (WS) ->

  messengerUrl = "ws://localhost:8000/wsmessenger"

  subscribe: (topic, cb) =>
    ws = new WS(messengerUrl)
    ws.onopen  = => ws.send JSON.stringify subscribe: topic
    ws.onmessage = (ev) => cb((JSON.parse ev.data).payload)
    return ws
