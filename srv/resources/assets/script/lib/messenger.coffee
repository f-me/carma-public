define ["lib/ws"], (WS) ->

  if window.location.protocol == "https:"
    messengerUrl = "wss://#{location.hostname}:8000/wsmessenger"
  else
    messengerUrl = "ws://#{location.hostname}:8000/wsmessenger"

  sendSubscribe = (ws, topic) => ws.send JSON.stringify subscribe: topic

  subscribe =  (topic, cb) =>
    ws = new WS(messengerUrl)
    ws.onopen  = => sendSubscribe(ws, topic)
    ws.onmessage = (ev) => cb((JSON.parse ev.data).payload)
    return ws


  multisubKVM: (kvms) =>
    ws = new WS(messengerUrl)
    cachekv = {}
    ws.onopen = => for k in ko.utils.unwrapObservable(kvms) when k.id()
      topic = "#{k._meta.model.name}:#{k.id()}"
      cachekv[topic] = k
      sendSubscribe ws, topic
    ws.onmessage = (ev) =>
      data = JSON.parse ev.data
      cachekv[data.topic]?._meta?.q?.saveSuccessCb?(_.identity)(data.payload)
    ws.onclose = =>
      cachekv = {}
    return ws

  subscribeKVM: (kvm, updatekvm) =>
    sub = _.once subscribe
    if kvm.id()?
      console.log "#{kvm._meta.model.name}:#{kvm.id()}"
      sub "#{kvm._meta.model.name}:#{kvm.id()}", updatekvm
    else
      throw "can't subscribe to model withoud id"

  subscribe: subscribe
