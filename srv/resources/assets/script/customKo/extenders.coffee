{ko} = require "carma/vendor"

ko.extenders.validate = (target, validator) ->

  # false - means no errors, it is valid
  # true  - means it isn't valid, error
  # or some string with error message
  target.validationError = ko.pureComputed -> validator target() ? false

  target
