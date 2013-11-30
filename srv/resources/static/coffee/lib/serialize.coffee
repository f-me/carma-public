define [], ->

  class SimpleSerialize

    isSerializable: (k, v) ->
      # do not serialize
      (not _.contains (_.keys SimpleSerialize.prototype), k) and
      ((ko.isWriteableObservable v) or not _.isFunction v)

    toJSON: (exclude) ->
      excl = (k) -> not _.contains exclude, k
      r = {}
      for k, v of @ when @isSerializable(k, v) and excl(k)
          n = ko.utils.unwrapObservable(v)
          r[k] = n if n
      r

    fromJSON: (obj) ->
      for k, v of obj
        if ko.isWriteableObservable @[k]
          @[k](v)
        else
          @[k] = v

  SimpleSerialize: SimpleSerialize
