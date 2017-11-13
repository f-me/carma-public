{_, ko} = require "carma/vendor"

# TODO explain what happens here
# TODO add some code examples to the explanation
ko.subscribable.fn.subscribeWithOld = (callback) ->
  oldValue = null
  @subscribe ((old) -> oldValue = old), this, 'beforeChange'
  @subscribe (newValue) -> callback(newValue, oldValue)

urlFor = (field) ->
  switch field.kvm._meta.model.name
    when "Case"
      "/#case/#{field()}"
    else
      "/##{field.kvm._meta.model.name}/#{field()}"

module.exports = {urlFor}
