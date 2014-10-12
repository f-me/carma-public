ko.subscribable.fn.subscribeWithOld = (callback) ->
    oldValue = null
    @subscribe ((old) -> oldValue = old), this, 'beforeChange'

    @subscribe (newValue) -> callback(newValue, oldValue)

window.urlFor = (field) ->
  switch field.kvm._meta.model.name
    when "Case"
      "/#case/#{field()}"
    else
      "/##{field.kvm._meta.model.name}/#{field()}"
