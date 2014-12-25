define [], ->
  callTypeBtn: (model, kvm) ->

    return unless kvm.callType

    callTypes = window.global.idents('CallType')
    oldValue = ko.observable(null)

    kvm.callType.subscribeWithOld (newVal, oldVal) -> oldValue(oldVal)

    kvm.callType.btn = ko.computed ->
      switch
        when kvm.callType() != callTypes['info']
          click: -> kvm.callType(callTypes['info'])
          text:  "Информационный звонок"
          visible: true
        when not _.isNull(oldValue())
          click: -> kvm.callType(oldValue())
          text: "Вернуть старое значение"
          visible: true
        else
          click: ->
          text: ""
          visible: false
