{$, _} = require "carma/vendor"
m = require "carma/dictionaries/meta-dict"

debounce = (wait, fn) -> _.debounce fn, wait

class AddressesDict extends m.dict
  constructor: (@opts)->
    @kvm = @opts.kvm
    @Dict = require "carma/dictionaries"
    @addressesDict = new @Dict.dicts.ModelDict
      dict: 'Addresses'

  find: debounce 1200, (q, cb, opt) ->
    return cb({}) if q.length < 4 and not opt?.force
    return cb(["aaaa", "aaaaaab"])

#    processResponse = (r) =>
#      @found = []
#
#      if _.isEmpty r
#        ["<span><i class='icon-ban-circle icon-white'></i>&nbsp;Ничего не найдено :(</span>"]
#      else
#        for i in r
#          do (i) =>
#            # fields which matched search query
#            fields = _.filter _.keys(i), (f) ->
#              # upper-casing fixes bug when "contract identifier" is erased
#              # when you select a contract from popup list while your input
#              # contains at least one lowercase symbol.
#              i[f] && String(i[f]).toUpperCase().indexOf(q.toUpperCase()) != -1
#            @found.push
#              id: i.id
#              matched: _.pick(i, fields)
#            @contr2html i, fields, q

    fetchResults = (d) ->
      if !d.nonEmpty
        cb [inlineSpinner "<div class='inline-spinner'></div>"]
      $.getJSON query, (res) ->
        d.nonEmpty = !_.isEmpty res
        items = processResponse res
        d.orig = _.clone items
        cb items
        # Show trailing spinner if ARC query is pending
        if d.needArc
          items.push inlineSpinner "<div class='inline-spinner'></div>"
          cb items

    # We explicitly pass dictionary object so that it is accessible
    # inside fetchResults (we can store original search response
    # this way to use later after ARC loading finishes)
    firstFetch = fetchResults this

    if @needArc
      parentThis = this
      $.getJSON arcQuery
        .always () ->
          parentThis.needArc = false
        .done (ar) ->
          # New contracts? Repeat contract search.
          if ar[0] > 0
            firstFetch.done () ->
              items = parentThis.orig
              items.push inlineSpinner "<div class='inline-spinner'></div>"
              cb items
              fetchResults parentThis
          # Remove trailing spinner
          else
            if parentThis.orig?
              cb parentThis.orig

  # returns html representation of an address
  contr2html: (c, fs = [], q = "") ->
    # make values human readable
    c.make  = @carMakeDict.getLab c.make || c.make
    c.model = @carModelDict.getLab c.model || c.model

    if c.subprogram
      subprogramDict = new @Dict.dicts.ComputedDict
        dict: "prefixedSubPrograms"
      c.subprogram = subprogramDict.getLab c.subprogram

    c._expired = do ->
      if _.isNull c._expired
        ""
      if c._expired
        "<span class='label label-danger'>Проверить участие</span>"
      else
        "<span class='label label-success'>Проверить условия</span>"

    # highlight matched search string
    if (not _.isEmpty fs) and q
      _.each fs, (f) ->
        return if f == "id"
        c[f] = "<span>#{c[f].replace(q, "<span class='finded'>#{q}</span>")}</span>"

    # required fields
    req = ["fromArc"
         , "vin"
         , "make"
         , "model"
         , "startMileage"
         , "validSince"
         , "_expired"
         , "subprogram"
         ]
    html = ""
    # show matched and required fields
    _.each _.union(req, fs), (f) ->
      if c[f]
        switch f
          when "fromArc"
            if c[f]
              html += "<span class='label'>ARC</span> "
          when "vin"
            html += "<b>#{c[f]}</b>"
          when "_expired"
            html += " #{c[f]}"
          when "startMileage"
            html += " [ #{c[f]} км ]"
          when "subprogram"
            html += "<br/>#{c[f]}<br/>"
          when "validSince"
            html += " <i>#{c[f]}</i>"
          else
            html += " #{c[f]}"
    html

  id2val: (i) ->
    return
    #return unless @found[i]
    # notify @kvm what contract was changed
    #@kvm.contract(@found[i].id)
    #_.chain(@found[i].matched).values().first().value()

module.exports =
  dict: AddressesDict
  name: 'AddressesDict'
