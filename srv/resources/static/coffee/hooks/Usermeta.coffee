define [], ->

  stateStuff: (model, kvm) =>
    kvm.toggleDelayed = (st) =>
      if kvm.delayedState() == st
        kvm.delayedState null
      else
        kvm.delayedState(st)

    kvm.inSBreak = ko.computed =>
      kvm.currentState() == 'ServiceBreak' or
      kvm.delayedState() == 'ServiceBreak'

    kvm.toggleServiceBreak = =>
      if kvm.currentState() == 'ServiceBreak'
        kvm.delayedState('Ready')
      else
        kvm.delayedState('ServiceBreak')
