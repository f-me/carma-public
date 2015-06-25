define ["lib/ws"], (WS) ->
  # CTI core
  class CTI
    constructor: (@extension) ->
      console.log "Enabling CTI for extension #{extension}"

      if window.location.protocol == "https:"
        url = "wss://#{location.host}/avaya/ws/#{extension}"
      else
        url = "ws://#{location.host}/avaya/ws/#{extension}"

      @ws = new WS(url)

    makeCall: (number) ->
      rq =
        action: "MakeCall"
        number: 0
      # Avoid using parseInt because Number.MAX_SAFE_INTEGER may be
      # less than what we need
      @ws.send JSON.stringify(rq).replace("0", number)

    endCall: (callId) ->
      @ws.send JSON.stringify
        action: "EndCall"
        callId: callId

    holdCall: (callId) ->
      @ws.send JSON.stringify
        action: "HoldCall"
        callId: callId

    retrieveCall: (callId) ->
      @ws.send JSON.stringify
        action: "RetrieveCall"
        callId: callId

    answerCall: (callId) ->
      @ws.send JSON.stringify
        action: "AnswerCall"
        callId: callId

    conferenceCall: (activeCall, heldCall) ->
      @ws.send JSON.stringify
        action: "ConferenceCall"
        activeCall: activeCall
        heldCall:   heldCall

    transferCall: (activeCall, heldCall) ->
      @ws.send JSON.stringify
        action: "TransferCall"
        activeCall: activeCall
        heldCall:   heldCall

    sendDigits: (callId, digits) ->
      @ws.send JSON.stringify
        action: "SendDigits"
        callId: callId
        digits: digits
