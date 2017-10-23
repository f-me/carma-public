require "carma/vendor"
require "carma/globallibs"
require "carma-styles"

data  = require "carma/data"
local = require "carma/local"

data.request().then ({dicts, user, users}) ->
  # DOM is already ready
  # (because scripts connected at bottom of the page).
  local.init {dicts, user, users}
