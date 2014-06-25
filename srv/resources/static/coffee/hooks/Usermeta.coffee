define [], ->

  stateStuff: (model, kvm) =>
    kvm.toggleDelayed = (st) =>
      unless _.isFunction(kvm.delayedState)
        return console.error("Need permission on delayedState")
      if kvm.delayedState() == st
        kvm.delayedState null
      else
        kvm.delayedState(st)

    kvm.inSBreak = ko.computed =>
      if not (_.isFunction(kvm.currentState) and _.isFunction(kvm.delayedState))
        return console.error("Need permission on delayedState and currentState")
      kvm.currentState() == 'ServiceBreak' or
      kvm.delayedState() == 'ServiceBreak'

    kvm.toggleServiceBreak = =>
      if not (_.isFunction(kvm.currentState) and _.isFunction(kvm.delayedState))
        return console.error("Need permission on delayedState and currentState")
      if kvm.currentState() == 'ServiceBreak'
        kvm.delayedState('Ready')
      else if kvm.delayedState() == 'ServiceBreak'
        kvm.delayedState(null)
      else
        kvm.delayedState('ServiceBreak')
