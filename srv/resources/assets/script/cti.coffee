class CTI
  constructor: (extension) ->
    if global.config("csta-ws-host")?
      host = global.config("csta-ws-host")
    else
      host = location.hostname

    port = global.config("csta-ws-port")

    if window.location.protocol == "https:"
      url = "wss://#{host}:#{port}/#{extension}"
    else
      url = "ws://#{host}:#{port}/#{extension}"

    @wsHandler = () ->

    @ws = new WebSocket(url)
    @ws.onmessage = (raw) =>
      msg = JSON.parse raw.data
      @wsHandler msg

  makeCall: (number) ->
    @ws.send JSON.stringify
      action: "MakeCall"
      number: number

  endCall: (callId) ->
    @ws.send JSON.stringify
      action: "EndCall"
      callId: callId

  answerCall: (callId) ->
    @ws.send JSON.stringify
      action: "AnswerCall"
      callId: callId


class CTIPanel
  constructor: (extension, el) ->
    console.log "Enabling CTI for extension #{extension}"
    cti = new CTI(extension)
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

    @cti.wsHandler = (msg) ->
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
            if not (RegExp("^#{extension}\:").test(msg.callingDevice))
              kvm.canAnswer true
              kvm.number msg.callingDevice.match(/\d+/)?[0]

          when "EstablishedEvent"
            kvm.canAnswer false

          when "ConnectionClearedEvent"
            kvm.number ""
            kvm.canCall true
            kvm.canEnd false
            kvm.callStart null

    el.find(".call-button").click () ->
      cti.makeCall kvm.number()

    el.find(".answer-button").click () ->
      cti.answerCall kvm.lastCallId()

    el.find(".end-button").click () ->
      cti.endCall kvm.lastCallId()

    el.show()
    ko.applyBindings kvm, el[0]

    $(document).keydown (e) ->
      if e.which == 192
        el.toggle()
