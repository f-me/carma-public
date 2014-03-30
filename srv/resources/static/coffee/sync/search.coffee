define ["sync/metaq", "sync/datamap", "map"], (metaq, m, map) ->
  class searchQ extends metaq
    constructor: (@kvm, @options) ->
      @api = @options.apiUrl
      @requestUrl = @api
      @model = @kvm._meta.model
      @searchFields = @options.defaultSort.fields
      @searchOrder  = @options.defaultSort.order
      @ftypes  = {}
      @ftypes[f.name] = f.type for f in @model.fields
      for f in @model.fields when not f.meta?.nosearch
        do (f) =>
          @kvm[f.name].subscribe (v) => @_search()
      if @kvm._meta.pager
        pager = @kvm._meta.pager
        pager.offset.subscribe (v) =>
          @requestUrl =
            "#{@api}?limit=#{pager.limit()}&offset=#{pager.offset()}"
          @_search()
        pager.offset.valueHasMutated()
      @kvm['searchResultsSpinner'] = ko.observable false

    _search: _.debounce((-> @search()), 300)

    search: =>
      q = {}
      for f in @model.fields when not f.meta?.nosearch
        if (_.isNumber @kvm[f.name]()) || (!_.isEmpty @kvm[f.name]())
          q[f.name] = @kvm[f.name]()
      preds =  m.c2sObj(q, @ftypes)
      sorts = { fields: @searchFields, order: @searchOrder }
      # have nothing to search, maybe user delete value
      # return @kvm['searchResults']([]) if _.isEmpty req
      $.ajax
        url      : @requestUrl
        dataType : 'json'
        type     : 'POST'
        data     : JSON.stringify { predicates: preds, sorts: sorts }
        success  : @successCb
        error    : @errorCb
        beforeSend : @showSpinner

    sort: (fs, ord) ->
      @searchFields = fs
      @searchOrder  = ord
      @_search()

    showSpinner: =>
      @kvm['searchResultsSpinner'] true

    hideSpinner: (jqXHR, error) =>
      if error
        alertUser "Данные не были получены. Повторите поиск."
      else
        @kvm['searchResultsSpinner'] false

    successCb: (data) =>
      @kvm['searchResults'](data)
      @hideSpinner()

    errorCb: (x, status) =>
      @hideSpinner x, status
      console.error "searchQ: search failed with
 '#{x.status}: #{x.statusText}'"

  searchQ: searchQ
