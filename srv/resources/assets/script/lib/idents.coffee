{Ajax} = require "carma/lib/ajax"

# Client-side idents manager with caching
class Idents extends Ajax
  getIdents: (modelName) ->
    retval = {}
    @bgetJSON "/cfg/idents/#{modelName}", (objs) -> retval = objs
    retval

mgr = new Idents

module.exports =
  idents: (m) -> mgr.getIdents m
