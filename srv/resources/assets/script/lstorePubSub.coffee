define [], ->

  # Pub/sub implementation, working thru localStorage, all messages will be
  # JSON.stringifyed before send and JSON.parsed before receive
  class LstoreSub
    constructor: ->
      s = localStorage
      @subs = {}
      window.addEventListener("storage", @notify, false);

    # cleanup all old ^lstore|.* keys, kind of gc
    cleanup: =>
      s = localStorage
      s.removeItem k for k of s when k.match /^lstore/

    # encode localstore keys as "lstore|real-sub-value" so it
    # won't mess with other stored content
    notify: (e) =>
      {key, oldValue, newValue, url} = e
      v = key.match(/^(lstore\|)(.*)/)
      # check newValue here because all values that we want to propagate to
      # subscribers will be serialized to json, but we still receive events
      # when key is removed, that we don't wont subscribers to receive
      return unless (v and newValue)
      [_, _, k] = v
      @subs[k]?(JSON.parse(newValue), JSON.parse(oldValue), url)
      # cleanup localstore from key
      # it may be a bad idea to destroy message here because some tabs
      # may not catch message, but for now it's only 1 tab, so it's ok
      localStorage.removeItem key

    # Subscribe
    # Arguments:
    # - key: topic to be subscribed
    # - fn: callback, which will receive message, it's arguments will be
    # newValue and oldValue
    sub: (key, fn)    => @subs[key] = fn

    # Publish
    # Arguments:
    # - key: topic
    # - value: anything that JSON.stringify can handle
    pub: (key, value) => localStorage["lstore|#{key}"] = JSON.stringify value
