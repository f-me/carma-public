define [], ->

  class LstoreSub
    constructor: ->
      s = localStorage
      @subs = {}
      @cleanup()
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
      return unless v
      [_, _, k] = v
      @subs[k]?(newValue, oldValue, url)

    sub: (key, fn)    => @subs[key] = fn

    pub: (key, value) => localStorage["lstore|#{key}"] = value
