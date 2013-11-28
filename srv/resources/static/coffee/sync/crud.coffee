define ["sync/metaq", "sync/datamap"], (metaq, m) ->
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
      @fetch() if @persisted
      @subscribe()
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

    save: (cb) =>
      cb ?= _.identity # just to be sure we have something to call
      @saveKvm() unless @persisted
      delete @q[k] for k, v of @q when @lastFetch[k] == v
      method = if @persisted then "PUT" else "POST"
      url    = if @persisted then "#{@url}/#{@kvm.id()}" else @url
      return cb(@kvm, @model) if _.isEmpty @q
      @qbackup = _.clone(@q)
      @q       = {}
      $.ajax
        type     : method
        url      : url
        dataType : 'json'
        success  : @saveSuccessCb(cb)
        error    : @saveErrorCb
        data     : JSON.stringify m.c2sObj(@qbackup, @ftypes)

    updadeKvm: (obj) =>
      @lastFetch = {}
      for k, v of obj
        @lastFetch[k] = _.clone v
        @kvm[k]?(v)

    saveSuccessCb: (cb) => (json) =>
      @persisted ||= true
      @updadeKvm(m.s2cObj(json, @ftypes))
      @qbackup = {}
      cb(@kvm, @model)

    saveErrorCb: (x, status) =>
      @q = _.defaults(@q, @qbackup)
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