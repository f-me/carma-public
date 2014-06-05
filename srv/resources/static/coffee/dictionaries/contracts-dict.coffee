define ["dictionaries/meta-dict", "dictionaries"], (m) ->
  class ContractsDict extends m.dict
    constructor: (@opts)->
      @kvm = @opts.kvm
      @Dict = require "dictionaries"
      @carMakeDict = new @Dict.dicts.ModelDict
        dict: 'CarMake'
      @carModelDict = new @Dict.dicts.ModelDict
        dict: 'CarModel'

    find: (q, cb, opt) ->
      return cb({}) if q.length < 4 and not opt?.force
      query = "/searchContracts/?query=#{q}&case=#{@kvm.id?()}&program=#{@kvm.program?()}&subprogram=#{@kvm.subprogram?()}#{if opt?.force then '&type=exact' else ''}"
      cb([inlineSpinner "<div class='inline-spinner'></div>"])
      $.getJSON query, (r) =>
        @found = []

        a = if _.isEmpty r
            ["<span><i class='icon-ban-circle icon-white'></i>&nbsp;Ничего не найдено :(</span>"]
          else
            for i in r
              do (i) =>
                # fields which matched search query
                fields = _.filter(_.keys(i), (f) ->
                  i[f] && String(i[f]).indexOf(q) != -1)
                @found.push
                  id: i.id
                  matched: _.pick(i, fields)
                @contr2html i, fields, q
        cb(a)

    # returns html representation of contract
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
          "<span class='label label-important'>Просрочен</span>"
        else
          "<span class='label label-success'>Действует</span>"

      # highlight matched search string
      if (not _.isEmpty fs) and q
        _.each fs, (f) ->
          return if f == "id"
          c[f] = "<span>#{c[f].replace(q, "<span class='finded'>#{q}</span>")}</span>"

      # required fields
      req = ["vin"
           , "make"
           , "model"
           , "startMileage"
           , "validSince"
           , "_expired"
           , "subprogram"]
      html = ""
      # show matched and required fields
      _.each _.union(req, fs), (f) ->
        if c[f]
          switch f
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
      return unless @found[i]
      # notify @kvm what contract was changed
      @kvm.contract(String(@found[i].id))
      _.chain(@found[i].matched).values().first().value()

  dict: ContractsDict
