define ["lib/local-dict"], (m) ->
  class ComputedDict extends m.dict
    constructor: (@opts, readycb) ->
      [f, a] = @opts.dict.split ':'
      fn    = @[f.trim()]
      throw new Error("Unknown dictionary #{f.trim}") unless fn
      args  = a.split(',').map((e) -> e.trim()) unless _.isEmpty args
      console.log 'cdict'
      fn.call(@, args)

    getLab: (val) -> @dictValues()[val]

  # Dictionary of all user-created programs
    allPrograms: =>
      $.bgetJSON "/all/program", (objs) =>
        @source = for obj in objs
          { value: obj.id.split(':')[1], label: obj.label || '' }

    programsVinEntries: =>
      $.bgetJSON "/all/program", (objs) =>
        @source = for obj in objs
               { value: obj.vinFormat
                 label: obj.label || ''
                 pname: obj.id.split(':')[1]
               }

  dict: ComputedDict