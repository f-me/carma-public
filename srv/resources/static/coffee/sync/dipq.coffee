define ["sync/metaq", "sync/datamap"], (metaq, m) ->
  class DipQueue extends metaq
    constructor: (@kvm, @model, @options) ->
      @api = "/geo/partners"
      @ftypes  = {}
      @ftypes[f.name] = f.type for f in @model.fields

      for f in @model.fields when not f.meta?.nosearch
        do (f) =>
          @kvm[f.name].subscribe (v) => @_search()

    _search: _.debounce((-> @search()), 300)

    search: =>
      a = @kvm["mapA"]
      b = @kvm["mapB"]
      if a? && b?
        @url = "#{@api}/#{a.lon},#{a.lat}/#{b.lon},#{b.lat}/"
      else
        @url = "#{@api}/1,1/100,100/"
      q = {}
      for f in @model.fields when @kvm[f.name]() and not f.meta?.nosearch
        q[f.name] = @kvm[f.name]()
      $.ajax
        url      : @url
        dataType : 'json'
        data     : m.c2sObj(q, @ftypes)
        success  : @successCb
        error    : @errorCb

    successCb: (data) => @kvm['searchResults'](data)

    errorCb: (x, status) =>
      console.error "dipq: search failed with '#{x.status}: #{x.statusText}'"

  DipQueue: DipQueue