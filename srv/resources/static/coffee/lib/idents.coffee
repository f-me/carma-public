define ["lib/ajax"], (Ajax) ->
  # Client-side idents manager with caching
  #
  # Wraps all ident integers in strings for backwards compatibility
  # with client
  class Idents extends Ajax
    getIdents: (modelName) ->
      retval = {}
      @bgetJSON "/cfg/idents/#{modelName}", (objs) -> retval = objs
      retval
