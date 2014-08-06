ko.subscribable.fn.subscribeWithOld = (callback) ->
    oldValue = null
    @subscribe ((old) -> oldValue = old), this, 'beforeChange'

    @subscribe (newValue) -> callback(newValue, oldValue)

window.urlFor = (kvm, name) ->
  switch kvm._meta.model.name
    when "Case"
      "/#case/#{kvm[name]()}"
    else
      "/##{kvm._meta.model.name}/#{kvm[name]()}"
