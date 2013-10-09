define ["sync/metaq", "sync/datamap", "map"], (metaq, m, map) ->
  class ServicesSearchQ extends metaq
    constructor: (@kvm, @model, @options) ->
      @api = "/search/services"
      @ftypes  = {}
      @ftypes[f.name] = f.type for f in @model.fields

      for f in @model.fields when not f.meta?.nosearch
        do (f) =>
          @kvm[f.name].subscribe (v) => @_search()

    _search: _.debounce((-> @search()), 300)

    search: =>
      @url = @api
      q = {}
      for f in @model.fields when @kvm[f.name]() and not f.meta?.nosearch
        q[f.name] = @kvm[f.name]()
      $.ajax
        url      : @url
        dataType : 'json'
        type     : 'POST'
        data     : JSON.stringify m.c2sObj(q, @ftypes)
        success  : @successCb
        error    : @errorCb

    successCb: (data) => @kvm['searchResults'](data)

    errorCb: (x, status) =>
      console.error "ServicesSearchQ: search failed with
 '#{x.status}: #{x.statusText}'"

  ServicesSearchQ: ServicesSearchQ