define ["lib/ajax"], (Ajax) ->
  # Client-side idents manager with caching
  class Idents extends Ajax
    getIdents: (modelName) ->
      retval = {}
      @bgetJSON "/cfg/idents/#{modelName}", (objs) -> retval = objs
      retval

  mgr = new Idents
  idents: (m) -> mgr.getIdents m
