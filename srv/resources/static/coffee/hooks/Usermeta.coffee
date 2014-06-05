define [], ->

  toggleDelayed: (model, kvm) =>
    kvm.toggleDelayed = (st) =>
      if kvm.delayedState() == st
        kvm.delayedState null
      else
        kvm.delayedState(st)

