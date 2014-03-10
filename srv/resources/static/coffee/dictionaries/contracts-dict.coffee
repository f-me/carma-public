define ["dictionaries/meta-dict", "dictionaries"], (m) ->
  class ContractsDict extends m.dict
    constructor: (@opts)->
      @kvm = @opts.kvm
      @Dict = require "dictionaries"
      @carMakeDict = new @Dict.dicts.ModelDict
        dict: 'CarMake'
      @carModelDict = new @Dict.dicts.ModelDict
        dict: 'CarModel'

    find: (q, cb) ->
      return cb({}) if q.length < 4
      query = "/searchContracts/?query=#{q}&program=#{@kvm.program()}&subprogram=#{@kvm.subprogram()}"
      $.getJSON query, (r) =>
        @found = _.map r, (c) ->
          id: c.id
          vin: c.vin

        a = for i in r
          do (i) =>
            # fields which matched search query
            fields = _.filter(_.keys(i), (f) ->
              i[f] && String(i[f]).indexOf(q) != -1)
            @contr2html i, fields, q
        cb(a)

    # returns html representation of contract
    contr2html: (c, fs = [], q = "") ->
      # make values human readable
      c.make  = @carMakeDict.getLab c.make || c.make
      c.model = @carModelDict.getLab c.model || c.model
      c._expired = do ->
        if _.isNull c._expired
          ""
        if c._expired
          "<span class='label label-important pull-right'>Просрочен</span>"
        else
          "<span class='label label-success pull-right'>Действует</span>"

      # highlight matched search string
      if (not _.isEmpty fs) and q
        _.each fs, (f) ->
          return if f == "id"
          c[f] = "<span>#{c[f].replace(q, "<span class='finded'>#{q}</span>")}</span>"

      # required fields
      req = ["vin", "make", "model", "_expired"]
      html = ""
      # show matched and required fields
      _.each _.union(req, fs), (f) ->
        if c[f]
          switch f
            when "vin"
              html += "<b>#{c[f]}</b>"
            when "_expired"
              html += "#{c[f]}"
            else
              html += "<br/>#{c[f]}"
      html

    id2val: (i) ->
      # notify @kvm what contract was changed
      @kvm.contract(String(@found[i].id))
      @found[i].vin

  dict: ContractsDict
