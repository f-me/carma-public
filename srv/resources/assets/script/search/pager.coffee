define ["lib/serialize", "utils"], (Serialize, Utils) ->

  page = (offset, limit) -> Math.round(offset / limit) + 1

  updateCurrent = (kvm, limit) -> (v) ->
    kvm.current(current v, limit)

  class Pager extends Serialize.SimpleSerialize
    constructor: (@kvm, lim = 10, offset = 0) ->

      @kvm._meta.pager = @

      @limit = ko.observable(lim)
      @offset = ko.observable(offset)
      @page   = ko.computed
        read:      => page @offset(), @limit()
        write: (p) => @offset((p-1) * @limit())

      @next = ko.observable()
      @prev = ko.observable()

    nextPage: => @offset(@next()) if _.isNumber @next()
    prevPage: => @offset(@prev()) if _.isNumber @prev()

    toJSON: -> super(['kvm'])


  buildPager: (kvm, lim, offset) -> new Pager(kvm, lim, offset)
