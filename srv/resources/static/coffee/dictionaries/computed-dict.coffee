define ["dictionaries/local-dict"], (ld) ->
  class ComputedDict extends ld.dict
    constructor: (@opts) ->
      [f, a] = @opts.dict.split ':'
      fn    = @[f.trim()]
      throw new Error("Unknown dictionary #{f.trim()}") unless fn
      args  = a.split(',').map((e) -> e.trim()) unless _.isEmpty args
      fn.call(@, args)

    getLab: (val) -> @dictValues()[val]

    # List of Role instances with isBack=true (used on #supervisor)
    backofficeRoles: =>
      @bgetJSON "/_/Role", (objs) =>
        @source = for obj in (_.filter objs, (o) -> o.isBack)
          { value: obj.id, label: obj.label || '' }

    # Dictionary of all usermetas with programManager role (used on
    # Program)
    programManagers: =>
      @bgetJSON "/_/Usermeta", (objs) =>
        pms = _.filter objs, (o) ->
          _.contains o.roles, String(global.idents("Role").programManager)
        @source = for p in pms
            { value: p.id
            , label: (p.realName + ' — ' + p.login) || ''
            }

    # Dictionary of all subprograms, with labels including parent
    # program name (used to assign users to subprograms)
    usermetaPrograms: =>
      @bgetJSON "/_/Program", (parentObjs) =>
        @bgetJSON "/_/SubProgram", (objs) =>
          @source = for obj in objs
            parent = _.find parentObjs, (p) -> p.id == obj.parent
            { value: String(obj.id)
            , label: (parent.label + ' — ' + obj.label) || ''
            }

    # Dictionary of all programs available to user from VIN screen.
    # - partner may see only his own programs
    # - vinAdmin role may access all programs
    # - all other users may do nothing
    vinPrograms: =>
      all_pgms = new ComputedDict(dict: "usermetaPrograms").source
      # Requires user to reload the page to update list of available
      # programs
      user_pgms =
        if global.user.meta.programs
          global.user.meta.programs.split ','
        else
          []
      @source =
        if _.contains global.user.roles, global.idents("Role").partner
          _.filter(all_pgms,
                  (e) -> _.contains user_pgms, e.value)
        else
          if _.contains global.user.roles, global.idents("Role").vinAdmin
            all_pgms
          else
            []

    Priorities: => @source = [1..3].map (e) -> s=String(e);{value:s,label:s}

  dict: ComputedDict
