class CTI
  constructor: (@extension) ->
    console.log "Enabling CTI for extension #{extension}"

    if global.config("csta-ws-host")?
      host = global.config("csta-ws-host")
    else
      host = location.hostname

    port = global.config("csta-ws-port")

    if window.location.protocol == "https:"
      url = "wss://#{host}:#{port}/#{extension}"
    else
      url = "ws://#{host}:#{port}/#{extension}"

    # List of WS message subscribers
    @subscribers = []

    @ws = new WebSocket(url)
    @ws.onconnect =
    @ws.onmessage = (raw) =>
      msg = JSON.parse raw.data
      _.each @subscribers, (h) -> h msg

  subscribe: (handler) ->
    @subscribers.push handler

  makeCall: (number) ->
    @ws.send JSON.stringify
      action: "MakeCall"
      number: parseInt number

  endCall: (callId) ->
    @ws.send JSON.stringify
      action: "EndCall"
      callId: callId

  answerCall: (callId) ->
    @ws.send JSON.stringify
      action: "AnswerCall"
      callId: callId


class CTIPanel
  constructor: (cti, el) ->
    @cti = cti

    # Panel widget interaction data
    kvm =
      number      : ko.observable ""
      callStart   : ko.observable null
      lastCallId  : ko.observable null
      canCall     : ko.observable true
      canAnswer   : ko.observable false
      canEnd      : ko.observable false
    @kvm = kvm

    wsHandler = (msg) ->
      if msg.event?
        if msg.callId?
          kvm.lastCallId msg.callId

        switch msg.event
          when "DeliveredEvent"
            kvm.callStart true
            kvm.canCall false
            kvm.canEnd true
            # FIXME Workaround until we have access to agent calls
            # list through WS
            #
            # Inbound call?
            if not (RegExp("^#{cti.extension}\:").test(msg.callingDevice))
              kvm.canAnswer true
              kvm.number msg.callingDevice.match(/\d+/)?[0]
            else
              kvm.number msg.calledDevice.match(/\d+/)?[0]

          when "EstablishedEvent"
            kvm.canAnswer false

          when "ConnectionClearedEvent"
            kvm.number ""
            kvm.canCall true
            kvm.canEnd false
            kvm.callStart null

    @cti.subscribe wsHandler

    el.find(".call-button").click () ->
      kvm.callStart new Date
      cti.makeCall kvm.number()

    el.find(".answer-button").click () ->
      cti.answerCall kvm.lastCallId()

    el.find(".end-button").click () ->
      cti.endCall kvm.lastCallId()

    el.submit (e) ->
      e.preventDefault()

    el.show()
    ko.applyBindings kvm, el[0]

    $(document).keydown (e) ->
      if e.which == 192
        el.toggle()
