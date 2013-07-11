define ["sync/metaq", "sync/datamap"], (metaq, m) ->
  class DipQueue extends metaq
    constructor: (@kvm, @model, @options) ->
      @url = "/partners/search"
      @ftypes  = {}
      @ftypes[f.name] = f.type for f in @model.fields

      for f in @model.fields
        do (f) =>
          @kvm[f.name].subscribe (v) => @_search()

    _search: _.debounce((-> @search()), 300)

    search: =>
      JSON.stringify m.c2sObj(@qbackup, @ftypes)
      q = {}
      for f in @model.fields when @kvm[f.name]()
        q[f.name] = @kvm[f.name]()
      $.ajax
        url      : @url
        dataType : 'json'
        data     : JSON.stringify m.c2sObj(q, @ftypes)
        success  : @successCb
        error    : @errorCb

    successCb: (data) => @kvm['searchResults'](data)

    errorCb: (x, status) =>
      console.error "dipq: search failed with '#{x.status}: #{x.statusText}'"

  DipQueue: DipQueue