define ["sync/datamap"], (m) ->
  class CrudQueue
    constructor: (@kvm, @model, @options) ->
      @url = "/_/#{@model.name}"
      @q       = {}
      @qbackup = {}
      @ftypes  = {}
      @persisted = @kvm.id()?
      @ftypes[f.name] = f.type for f in @model.fields
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

    _save: _.debounce((-> @save()), 1300)

    save: (cb) =>
      method = if @persisted then "PUT" else "POST"
      url    = if @persisted then "#{@url}/#{@kvm.id()}" else @url
      @qbackup = _.clone(@q)
      @q       = {}
      $.ajax
        type     : method
        url      : url
        dataType : 'json'
        success  : @saveSuccessCb(cb)
        error    : @saveErrorCb
        data     : JSON.stringify m.c2sObj(@qbackup, @ftypes)

    updadeKvm: (obj) => @kvm[k]?(v) for k, v of obj

    saveSuccessCb: (cb) => (json) =>
      @persisted ||= true
      @updadeKvm(m.s2cObj(json, @ftypes))
      @qbackup = {}
      cb(@kvm, @model) if _.isFunction(cb)

    saveErrorCb: (x, status) =>
      @q = _.defaults(@q, @qbackup)
      @qbackup = {}
      console.error "CRUD sync: save of '#{@model.name}:#{@kvm.id()}' failed
 with '#{x.status}: #{x.statusText}'"

  CrudQueue: CrudQueue