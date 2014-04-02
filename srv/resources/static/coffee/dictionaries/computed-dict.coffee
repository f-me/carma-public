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
    prefixedSubPrograms: =>
      @bgetJSON "/_/Program", (parentObjs) =>
        @bgetJSON "/_/SubProgram", (objs) =>
          @source = for obj in objs
            parent = _.find parentObjs, (p) -> p.id == obj.parent
            { value: obj.id
            , label: (parent.label + ' — ' + obj.label) || ''
            }

    # Dictionary of all subprograms available to user from VIN/portal screen.
    # - partner/psaanalyst may see only his own programs
    # - contractAdmin/vinAdmin role may access all programs
    # - all other users see no programs
    #
    # TODO Compute this on server.
    portalSubPrograms: =>
      all_pgms = new ComputedDict(dict: "prefixedSubPrograms").source
      # Requires user to reload the page to update list of available
      # programs
      @source =
        if _.contains(global.user.roles, global.idents("Role").partner) or
           _.contains(global.user.roles, global.idents("Role").psaanalyst)
          user_pgms =
            if global.user.meta.programs
              _.map (global.user.meta.programs.split ','), (s) -> parseInt s
            else
              []
          _.filter(all_pgms,
                  (e) -> _.contains user_pgms, e.value)
        else
          if _.contains(global.user.roles, global.idents("Role").vinAdmin) or
             _.contains(global.user.roles, global.idents("Role").contract_admin)
            all_pgms
          else
            []

    allPartners: =>
      @bgetJSON "/all/partner", (objs) =>
        @source = for o in objs when o.name
          { value: o.id, label: o.name }

    Priorities: => @source = [1..3].map (e) -> s=String(e);{value:s,label:s}

    ExistDict: =>
      @source = [ {label: "Не задано", value: "unspec" }
                , {label: "Есть",      value: "yes"    }
                , {label: "Нет",       value: "no"     }
                ]

  dict: ComputedDict
