define ["lib/ajax"], (Ajax) ->
  # Client-side configuration manager with caching
  class Config extends Ajax
    getOption: (option) ->
      retval = {}
      @bgetJSON "/clientConfig", (objs) -> retval = objs[option]
      retval
