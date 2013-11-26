define [], ->
  class Ajax
    ajaxCacheEnabled: true

      # Synchronous JSON request
    bgetJSON: (url, cb) ->
      @ajax
        type     : 'GET'
        url      : url
        dataType : 'json'
        success  : cb
        async    : false


    getJson: (a, b, c) => @get(a,b,c,"json")

    get: (a, d, e, g) =>
      if _.isFunction(d)
        g=g||e
        e=d
        d=b
      @ajax({type:c,url:a,data:d,success:e,dataType:g})

    ajax: (args...) ->
      # just use old $.ajax when cache is disabled or when we not
      # in chacheable scope
      return $.ajax.apply(@, args) unless @ajaxCacheEnabled
      # args could be url, [settings] or just [settings]
      # [] - means mandatory
      # so I put them together in one object
      if _.isString args[0]
        h = if _.isObject args[1] then args[1] else {}
        h.url = args[0]
      else if _.isObject args[0]
        h = args[0]
      else
        throw new Error \
          "Man, what's wrong with you, wtf is this shit: #{JSON.stringify(args)}"

      # and here is real caching begins
      @constructor.ajacCache ?= {}
      cache = @constructor.ajacCache

      url = h.url
      succfn = h.success
      if (cache[url])
        console.log "cache hit: #{h.url}"
        setTimeout (-> succfn cache[url]), 0 if _.isFunction succfn
      else
        console.log "cache miss: #{h.url}", cache
        newsucc = (data) ->
          cache[url] = data
          succfn(data) if _.isFunction succfn
        h.success = newsucc
        $.ajax h
