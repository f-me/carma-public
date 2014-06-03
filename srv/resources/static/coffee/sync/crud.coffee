define [ "sync/metaq"
       , "sync/datamap"
       , "lib/Messenger"
       ], (metaq, m, Messenger) ->
  class CrudQueue extends metaq
    constructor: (@kvm, @model, @options) ->
      @url = "/_/#{@model.name}"
      @q       = {}
      @qbackup = {}
      @ftypes  = {}
      @persisted = @kvm.id()?
      @ftypes[f.name] = f.type for f in @model.fields
      @debounced_save = _.debounce((-> @save()), 1300)
      # lastfetch is keeping date that was fetched from server during
      # last push, it is used to prevent immediate pushback of just fetched
      # fields
      @lastFetch = {}
      # when we have id, first fetch data and only after that subscribe
      # to changes, fetch will block, so we won't get fetched data to the save
      # queue
      @fetch() if @persisted and not @options?.not_fetch
      @subscribe()

      # Example of ws crud updates listener
      # if @persisted
      #   Messenger.subscribe "#{model.name}:#{@kvm.id()}",
      #                       @saveSuccessCb(_.identity)
      @

    subscribe: =>
      for f in @model.fields
        do (f) =>
          @kvm[f.name].subscribe (v) =>
            @q[f.name] = v
            @._save() unless @options?.manual_save

    fetch: =>
      $.bgetJSON "#{@url}/#{@kvm.id()}", (o) => @updadeKvm m.s2cObj(o, @ftypes)

    _save: => @debounced_save()

    # we have cases with new models when lastFetch is still empty
    # but we allready wrote null in some field, we don't need to
    # actually save such data, anyway it's empty
    # other case is when fetch failed, but some code initialize
    # itself with nulls, we also don't want to break data it such case
    # issue #1568
    haveBefore: (b, a) => b == a or ((_.isUndefined b) and (_.isNull a))

    save: (cb, force = false) =>
      cb ?= _.identity # just to be sure we have something to call
      @saveKvm() unless @persisted
      delete @q[k] for k, v of @q when @haveBefore(@lastFetch[k], v)
      method = if @persisted then "PUT" else "POST"
      url    = if @persisted then "#{@url}/#{@kvm.id()}" else @url
      return cb(@kvm, @model) if (_.isEmpty @q) and not force
      @qbackup = _.clone(@q)
      @q       = {}
      $.ajax
        type     : method
        url      : url
        dataType : 'json'
        contentType:'application/json; charset=utf-8'
        success  : @saveSuccessCb(cb)
        error    : @saveErrorCb
        data     : JSON.stringify m.c2sObj(@qbackup, @ftypes)
        beforeSend : @showSyncAnim

    showSyncAnim: =>
      _.each (_.keys @qbackup), (fname) =>
        @kvm["#{fname}Sync"] true

    hideSyncAnim: (jqXHR, status) =>
      if status
        alertUser "Данные не были сохранены. Попробуйте сохранить изменения ещё раз."
      else
        _.each (_.keys @qbackup), (fname) =>
          @kvm["#{fname}Sync"] false

    updadeKvm: (obj) =>
      # Hope This won't break anything, erasing last fetch don't work
      # with ws notifications because we may receive 2 updates after
      # put first from crud second from ws queue and second update
      # will be immediately put back
      # @lastFetch = {}
      for k, v of obj
        @lastFetch[k] = _.clone v
        @kvm[k]?(v)

    saveSuccessCb: (cb) => (json) =>
      @persisted ||= true
      @updadeKvm(m.s2cObj(json, @ftypes))
      @hideSyncAnim()
      @qbackup = {}
      cb(@kvm, @model)

    saveErrorCb: (x, status) =>
      @q = _.defaults(@q, @qbackup)
      @hideSyncAnim(x, status)
      @qbackup = {}
      console.error "CRUD sync: save of '#{@model.name}:#{@kvm.id()}' failed
 with '#{x.status}: #{x.statusText}'"

    # push all non empty kvm fields to the save queue, on first post for example
    saveKvm: =>
      for f in @model.fields when @kvm[f.name]()
        @q[f.name] = @kvm[f.name]()

    # sync new model, if it's not persisted(have no id), then just save it
    # in other case first save all that we have and then fetch data from backend
    sync: (cb) =>
      cb ?= _.identity
      unless @persisted
        @safeKvm
        return @save(cb)
      @save => @fetch(); cb(@kvm, @model)

  CrudQueue: CrudQueue
