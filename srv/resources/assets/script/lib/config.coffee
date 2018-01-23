{Ajax} = require "carma/lib/ajax"

# Client-side configuration manager with caching
module.exports.Config = class Config extends Ajax
  getOption: (option) ->
    retval = {}
    @bgetJSON "/clientConfig", (objs) -> retval = objs[option]
    retval
