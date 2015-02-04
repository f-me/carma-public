class CTI
  constructor: (@extension) ->
    console.log "Enabling CTI for extension #{extension}"

    if window.location.protocol == "https:"
      url = "wss://#{location.host}/avaya/ws/#{extension}"
    else
      url = "ws://#{location.host}/avaya/ws/#{extension}"

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

class CTIPanel
  constructor: (cti, el) ->
    # CTI panel state (it's a bit different from agent state in CSTA
    # lib to make interface coding easier)
    kvm =
      # Rows in CTI panel (array of CallVM's)
      calls: ko.observableArray []
      # CallId to extension mapping (used to keep extension number
      # values between state changes)
      extensions: {}
      # A call in progress (unknown to server yet). Shown instead of
      # the blank call line.
      wipCall: null
      # Show an extra line for a new call
      showBlankCall: ko.observable false

    displayedToInternal = (number) ->
      number.replace("+7", "98").replace("+", "9810")

    internalToDisplayed = (number) ->
      number?.match(/\d+/)?[0]?.replace(/^(98|8|)(\d{10})$/, "\+7$2")

    # Update kvm from state reported by the service
    stateToVM = (state) ->
      # VM for a call. If callId is null, return fresh VM for empty
      # CTI panel
      class CallVM
        constructor: (call, callId) ->
          @prev       = ko.observable null
          @number     =
            ko.observable(
              _.map(call.interlocutors, internalToDisplayed).join("\n"))
          @callStart  = ko.observable call.start?
          @callId     = callId
          # Extension number typed so far
          @extension  = ko.computed
            write:
              (v) ->
                # Send only new digits to csta-ws
                old = kvm.extensions[callId] || ""
                kvm.extensions[callId] = v
                if v.length > old.length
                  diff = v.length - old.length
                  cti.sendDigits callId, v.substr(old.length, diff)
            read:
              -> kvm.extensions[callId] || ""
          # How many rows are in number field
          @numberRows =
            if _.isEmpty call.interlocutors
              1
            else
              call.interlocutors.length
          # Work in progress indicator
          @wip = ko.observable false

          # canX are observable, because we want to hide buttons from
          # the panel even before the service reports new call
          # state/event
          @canExtend  = ko.observable(
            call.answered? && call.direction == "Out" && !call.held)
          @canCall    = ko.observable !(callId?)
          @canAnswer  = ko.observable(
            !(call.answered?) && (call.direction == "In"))
          @canHold    = ko.observable (call.answered? && !call.held)
          @canRetrieve= ko.observable call.held
          @canConf    = ko.computed =>
            @prev?() && call.answered? && !call.held
          @canTransfer= ko.computed =>
            @prev?() && call.answered? && !call.held
          @canInsta   = ko.computed => !@wip()
          @canEnd     = ko.observable(
            !call.held && (call.answered? || (call.direction == "Out")))

          @addBlankCall= -> kvm.showBlankCall true

          # Button click handlers
          @makeThis= ->
            return if _.isEmpty @number()
            if @prev()?
              cti.holdCall @prev().callId
            cti.makeCall displayedToInternal @number()
            @canCall false
            @wip true
            kvm.wipCall = this
            @callStart new Date().toISOString()
          @instaDial = (number) -> () ->
            if callId?
              cti.holdCall callId
              kvm.showBlankCall true
              targetVM = _.last kvm.calls()
            else
              targetVM = this
            targetVM.number number
            targetVM.makeThis()
          @answerThis= ->
            if @prev()?
              cti.holdCall @prev().callId
            cti.answerCall callId
            @canAnswer false
          @endThis= ->
            cti.endCall callId
            @canEnd false
          @holdThis= ->
            cti.holdCall callId
          @retrieveThis= ->
            cti.retrieveCall callId
          @confThis= ->
            cti.conferenceCall callId, @prev().callId
          @transferThis= ->
            cti.transferCall callId, @prev().callId

      newCalls = for callId, call of state.calls
        new CallVM call, callId
      # Hide call in progress if the server finally knows about it.
      # state.calls contains only calls from the server, but kvm.calls
      # always has an extra CallVM (blank line/call in progress)
      if _.keys(state.calls).length == kvm.calls().length
        kvm.wipCall = false
        kvm.showBlankCall false
      # Always add a "blank line"/"call in progress" for a new call
      if kvm.wipCall
        newCalls.push kvm.wipCall
      else
        newCalls.push new CallVM {}, null

      # Delete unknown extension digits
      for k in _.keys kvm.extensions
        if !_.contains(_.keys(state.calls), k)
          delete kvm.extensions[k]

      # Set links to "previous row" in every call
      for i of newCalls
        if i > 0
          newCalls[i].prev newCalls[i - 1]

      kvm.calls.removeAll()
      for c in newCalls
        kvm.calls.push c

    wsHandler = (msg) ->
      if msg.errorText?
        console.log "CTI: #{msg.errorText}"
        $.notify "Ошибка CTI: #{msg.errorText}"

      if msg.calls?
        stateToVM msg

      if msg.newState?
        stateToVM msg.newState

    cti.subscribe wsHandler

    el.show()
    ko.applyBindings kvm, el[0]

    # Allow only 0-9, * and # in extension number field
    $(el).on "keydown", ".extension-mask", (e) ->
      unless ((e.which >= 48 && e.which <= 57) ||
        (e.which >= 96 && e.which <= 105) ||
          e.which == 106 || e.which == 56 || e.which == 51)
        e.preventDefault()

    # Make call upon <Enter> in number field
    $(el).on "keydown", ".number", (e) ->
      if e.which == 13
        $(e.target).parent(".call-form").submit()
        e.preventDefault()

    $(document).keydown (e) ->
      if e.which == 192
        el.toggle()
