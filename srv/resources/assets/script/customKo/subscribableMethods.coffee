{ko} = require "carma/vendor"

# See https://stackoverflow.com/a/18184016/774228
# Useful for comparing old value with new value in listener.
ko.subscribable.fn.subscribeWithOld = (callback) ->
  oldValue = null
  @subscribe ((old) -> oldValue = old), this, "beforeChange"
  @subscribe (newValue) -> callback newValue, oldValue
