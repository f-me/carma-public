{_} = require "carma/vendor"
ld = require "carma/dictionaries/local-dict"

class ComputedDict extends ld.dict
  constructor: (@opts) ->
    [f, a] = @opts.dict.split ':'
    fn    = @[f.trim()]
    throw new Error("Unknown dictionary #{f.trim()}") unless fn
    args  = a.split(',').map((e) -> e.trim()) unless _.isEmpty args
    fn.call(@, args)

  # Use @allValuesMap to provide id-label mapping for non-selectable
  # items
  getLab: (val) -> (@allValuesMap || @dictValues())[val]

  # ServiceType dictionary including icon field and model names
  iconizedServiceTypes: =>
    @bgetJSON "/_/CtrModel", (mdls) =>
      @bgetJSON "/_/ServiceType", (styps) =>
        @source = for st in styps
          label: st.label || '',
          icon: st.icon,
          value: st.id,
          model: (_.find mdls, (ctr) -> st.model == ctr.id).value

  # List of Role instances with isBack=true (used on #supervisor)
  backofficeRoles: =>
    @bgetJSON "/_/Role?isBack=t&hidden=f", (objs) =>
      @source = for obj in objs
        { value: obj.id, label: obj.label || '' }

  # List of Role instances with hidden=false (used on BusinessRole &
  # users dict)
  visibleRoles: =>
    @bgetJSON "/_/Role?hidden=f", (objs) =>
      @source = for obj in objs
        { value: obj.id, label: obj.label || '' }

  # Dictionary of all usermetas with programManager role (used on
  # Program)
  programManagers: =>
    @bgetJSON "/_/Usermeta", (objs) =>
      pms = _.filter objs, (o) ->
        _.contains o.roles, window.global.idents("Role").programManager
      @source = for p in pms
          { value: p.id
          , label: p.realName || p.login
          }

  # Dictionary of all subprograms, with labels including parent
  # program name
  prefixedSubPrograms: =>
    @bgetJSON "/_/Program", (parentObjs) =>
      @bgetJSON "/_/SubProgram", (objs) =>
        @source = for obj in objs
          parent = _.find parentObjs, (p) -> p.id == obj.parent
          { value: obj.id
          , label: (parent.label + ' — ' + obj.label) || ''
          }

  # Dictionary of all car models, with labels including parent make
  # names
  prefixedModels: =>
    @bgetJSON "/_/CarMake", (makes) =>
      @bgetJSON "/_/CarModel", (models) =>
        @source = for model in models
          parent_make = _.find makes, (m) -> m.id == model.parent
          { value: model.id
          , label: (parent_make.label + ' — ' + model.label) || ''
          }

  # Dictionary of all car generations, with labels including parent
  # make and model names
  prefixedGenerations: =>
    @bgetJSON "/_/CarMake", (makes) =>
      @bgetJSON "/_/CarModel", (models) =>
        @bgetJSON "/_/CarGeneration", (gens) =>
          @source = for gen in gens
            parent_model = _.find models, (m) -> m.id == gen.parent
            parent_make = _.find makes, (m) -> m.id == parent_model.parent
            { value: gen.id
            , label: (parent_make.label + ' — ' + parent_model.label + ' — ' + gen.label) || ''
            }

  # Dictionary of all subprograms available to user from VIN/portal screen.
  # - partner/psaanalyst may see only his own subprograms
  # - contractAdmin/vinAdmin role may access all subprograms
  # - all other users see no subprograms
  #
  # TODO Compute this on server.
  portalSubPrograms: =>
    all_pgms = new ComputedDict(dict: "prefixedSubPrograms").source
    # Requires user to reload the page to update list of available
    # subprograms
    @source =
      if _.contains(window.global.user.roles, window.global.idents("Role").vinAdmin) or
         _.contains(window.global.user.roles, window.global.idents("Role").contract_admin)
        all_pgms
      else
        if _.contains(window.global.user.roles, window.global.idents("Role").partner) or
           _.contains(window.global.user.roles, window.global.idents("Role").psaanalyst)
          user_pgms = window.global.user.subPrograms || []
          _.filter(all_pgms, (e) -> _.contains user_pgms, e.value)
        else
          []

  # FIXME: maybe we can drop this and use `ModelDict:Partner` instead
  allPartners: =>
    @bgetJSON "/_/Partner?limit=7000", (objs) =>
      @source = for o in objs when o.name
        { value: o.id, label: o.name }

  diagTreeRoots: =>
    @bgetJSON "/_/DiagSlide?isRoot=t", (objs) =>
      @source = for obj in objs
        { value: obj.id, label: obj.header || '' }

  serviceSubtypes: =>
    @bgetJSON "/_/TowSort", (tow_types) =>
      @bgetJSON "/_/TechType", (tech_types) =>
        @bgetJSON "/_/BikeTowType", (bike_tow_types) =>
          sType = window.global.idents("ServiceType")
          enrich = (es, prefix, sType) ->
            for e in es
              { subtypeId: e.id, label: "#{prefix} - #{e.label}", typeId: sType }
          all_types =
            # TODO Use actual ServiceType labels here?
            enrich(tow_types, "Эвакуация", sType.towage).concat(
              enrich(_.filter(tech_types, (e) -> e.isActive), "Техпомощь", sType.tech).concat(
                enrich(bike_tow_types, "Мотоэвакуация", sType.bikeTowage)))
          # Re-number element of this dictionary. Original "subtype
          # ID" values are preserved in "subtypeID" property of
          # elements.
          @source = for e, i in all_types
            e.value = i + 1
            e
          @getElement = (v) -> _.find(@source, (e) -> e.value == v)
          @getValue = (t, s) ->
            _.find(@source, ({typeId, subtypeId}) ->
              typeId == t && subtypeId == s)?.value

  Priorities: => @source = [1..3].map (e) -> s=String(e);{value:s,label:s}

  ExistDict: =>
    @source = [ {label: "Не задано", value: "unspec" }
              , {label: "Есть",      value: "yes"    }
              , {label: "Нет",       value: "no"     }
              ]

  UserStateVal: =>
    vals =
      { LoggedOut:    ["Разлогинен",        "#888a85"]
      , Ready:        ["Готов",             "#ef2929"]
      , Busy:         ["Занят",             "#8ae234"]
      , Rest:         ["Перерыв",           "#fcaf3e"]
      , Dinner:       ["Обед",              "#e9b96e"]
      , ServiceBreak: ["Служебный перерыв", "#729fcf"]
      , NA:           ["NA",                "#72c6cf"]
      }
    @colors = _.reduce vals, ((m, [_, c], k) -> m[k] = c), {}
    @source = _.map vals, ([v, c], k) -> {label: v, value: k, color: c}

module.exports =
  dict: ComputedDict
  name: 'ComputedDict'
