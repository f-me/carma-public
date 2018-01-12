{_, ko} = require "carma/vendor"

{SimpleSerialize} = require "carma/lib/serialize"
Utils             = require "carma/utils"

page = (offset, limit) -> Math.round(offset / limit) + 1

updateCurrent = (kvm, limit) -> (v) ->
  kvm.current(current v, limit)

class Pager extends SimpleSerialize
  constructor: (@kvm, lim = 10, offset = 0) ->

    @kvm._meta.pager = @

    @limit  = ko.observable lim
    @offset = ko.observable offset

    @page = ko.computed
      read:      => page @offset(), @limit()
      write: (p) => @offset (p-1) * @limit()

    @prev = ko.observable()
    @next = ko.observable()
    @isPrevHidden = ko.pureComputed => ! _.isNumber @prev()
    @isNextHidden = ko.pureComputed => ! @next()

  nextPage: => @offset @next() if _.isNumber @next()
  prevPage: => @offset @prev() if _.isNumber @prev()

  toJSON: -> super ['kvm']


module.exports =
  buildPager: (kvm, lim, offset) -> new Pager(kvm, lim, offset)
