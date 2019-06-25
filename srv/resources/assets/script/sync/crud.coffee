{$, _, moment} = require "carma/vendor"

{MetaQueue} = require "carma/sync/metaq"
m           = require "carma/sync/datamap"
Messenger   = require "carma/lib/messenger"

class CrudQueue extends MetaQueue
  constructor: (@kvm, @model, @options = {}) ->
    @url = "/_/#{@model.name}"
    @q       = {}
    @qbackup = {}
    @ftypes  = {}
    @persisted = @kvm.id()?
    @ftypes[f.name] = f.type for f in @model.fields
    @debounced_save = _.debounce (=> do @save), 1300
    # lastfetch is keeping data from model previous state
    # last push, it is used to prevent immediate pushback of just fetched
    # fields
    @lastFetch = {}
    # when we have id, first fetch data and only after that subscribe
    # to changes, fetch will block, so we won't get fetched data to the save
    # queue
    do @fetch if @persisted and not @options?.dontFetch
    do @subscribe

    # use ws crud notifications
    if @options.useWS?
      if @persisted
        @ws = Messenger.subscribeKVM @kvm, @saveSuccessCb _.identity
      else
        @kvm.id.subscribe =>
          @ws = Messenger.subscribeKVM @kvm, @saveSuccessCb _.identity

  subscribe: =>
    for f in @model.fields
      do (f) =>
        @kvm[f.name].subscribe (v) =>
          # Silently skip saving case.program=null.
          # This is used when searching contract with program field cleared.
          if @model.name is 'Case' and f.name is 'program' and not v
            return
          # Substitute Action.deferBy with computed value (see #2617).
          if @model.name is 'Action' \
          and f.name is 'deferBy' \
          and v is '$expectedSvcStart$+5m'
            sid = @kvm.serviceId()
            @kvm._parent.servicesReference().forEach (svc) ->
              if String(svc.id()) is String(sid)
                fmt = 'DD.MM.YYYY HH:mm:ss'
                newDate =
                  moment(svc.times_expectedServiceStart(), fmt)
                    .add(5, 'minutes')
                diffDur = moment.duration newDate.diff moment()
                v = "#{Math.floor diffDur.asHours()}:#{
                  Math.abs Math.floor(diffDur.asMinutes() % 60)}"
          @q[f.name] = v
          do @_save unless @options?.manual_save

  fetch: =>
    $.bgetJSON "#{@url}/#{@kvm.id()}", (o) => @updateKvm m.s2cObj o, @ftypes

  # Actually gets all fields but updates in `kvm` only those which specified.
  # Maybe in the future on back-end could be implemented request handler
  # that could return only requested fields.
  fetchOnly: (fields) => # fields: [string]
    $.bgetJSON "#{@url}/#{@kvm.id()}", (o) =>
      newData = m.s2cObj o, @ftypes
      for fieldName in fields when o[fieldName]?
        @kvm[fieldName]? newData[fieldName]

  _save: => do @debounced_save

  save: (cb = _.identity, force = false, cbErr = _.identity) =>
    do @saveKvm unless @persisted
    delete @q[k] for k, v of @q when _.isEqual @lastFetch[k], v
    _.extend @lastFetch, $.extend true, {}, @q
    method = if @persisted then "PUT" else "POST"
    url    = if @persisted then "#{@url}/#{@kvm.id()}" else @url
    return cb @kvm, @model if _.isEmpty(@q) and not force
    @qbackup = $.extend @qbackup, _.clone @q
    @q = {}

    $.ajax {
      type:        method
      url
      dataType:    'json'
      contentType: 'application/json; charset=utf-8'
      success:     @saveSuccessCb cb
      error:       @saveErrorCb cbErr
      data:        JSON.stringify m.c2sObj @qbackup, @ftypes
      beforeSend:  @showSyncAnim
    }

  showSyncAnim: =>
    _.each (_.keys @qbackup), (fname) =>
      @kvm["#{fname}Sync"] true

  hideSyncAnim: (jqXHR, status) =>
    # clean errors from date fields
    _.each (_.keys @qbackup), (fname) =>
      if "#{fname}InvalidDate" of @kvm
        @kvm["#{fname}InvalidDate"] false

    if status
      if 'responseJSON' of jqXHR and 'validationFailure' of jqXHR.responseJSON
        for k, v of jqXHR.responseJSON.validationFields
          $.notify "Ошибка ввода данных: #{v}"
          if "#{k}InvalidDate" of @kvm
            @kvm["#{k}InvalidDate"] true
          else
            @kvm["#{k}InvalidDate"] false

        _.each (_.keys @qbackup), (fname) =>
          @kvm["#{fname}Sync"] false

      else
        $.notify "Данные не были сохранены.
                  Попробуйте сохранить изменения ещё раз."
    else
      _.each (_.keys @qbackup), (fname) =>
        @kvm["#{fname}Sync"] false

  updateKvm: (obj) =>
    # Hope This won't break anything, erasing last fetch don't work
    # with ws notifications because we may receive 2 updates after
    # put first from crud second from ws queue and second update
    # will be immediately put back
    # @lastFetch = {}
    a = {}
    for k, v of obj
      a[k] = v
      @kvm[k]?(v)
    _.extend @lastFetch, $.extend true, {}, a

  saveSuccessCb: (cb) => (json) =>
    @persisted ||= true
    @updateKvm m.s2cObj json, @ftypes
    @hideSyncAnim()
    @qbackup = {}
    @kvm._saveSuccessCb? @kvm, @model, json
    cb @kvm, @model

  saveErrorCb: (cbErr) => (x, status) =>
    @q = _.defaults @q, @qbackup
    @hideSyncAnim x, status
    console.error "CRUD sync: save of '#{@model.name}:#{@kvm.id()}' failed
                   with '#{x.status}: #{x.statusText}'"
    cbErr x, @kvm, @model

  # push all non empty kvm fields to the save queue, on first post for example
  saveKvm: =>
    for f in @model.fields when @kvm[f.name]()
      @q[f.name] = @kvm[f.name]()

  # sync new model, if it's not persisted(have no id), then just save it
  # in other case first save all that we have and then fetch data from backend
  sync: (cb) =>
    cb ?= _.identity
    unless @persisted
      @safeKvm # FIXME is this supposed to be a method? it isn' a call here
      return @save cb
    @save => @fetch(); cb @kvm, @model

  destructor: =>
    @ws.close # FIXME is this a method? so this is not a call

module.exports = {CrudQueue}
