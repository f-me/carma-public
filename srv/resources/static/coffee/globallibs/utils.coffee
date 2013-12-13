
window.urlFor = (kvm, name) ->
  switch kvm._meta.model.name
    when "Case"
      "/#case/#{kvm[name]()}"
    else
      "/##{kvm._meta.model.name}/#{kvm[name]()}"