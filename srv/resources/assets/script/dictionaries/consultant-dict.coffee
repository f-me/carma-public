define ["dictionaries/meta-dict"], (m) ->
  class ConsultantDict extends m.dict
    constructor: (@opts) ->
      r = window.global.idents("Role")
      consRoles = [r.consultant_op, r.consultant_mech, r.consultant_tech]
      users = _.filter window.global.dictionaries.users.entries, (u) ->
        u.isActive && _.intersection(u.roles, consRoles).length
      users.sort (a, b) -> if a.label > b.label then 1 else -1

      @ops   = (u for u in users when u.roles.indexOf(r.consultant_op)   != -1)
      @mechs = (u for u in users when u.roles.indexOf(r.consultant_mech) != -1)
      @techs = (u for u in users when u.roles.indexOf(r.consultant_tech) != -1)
      @vals  = []


    find: (q, cb, opt) ->
      q = q.trim().toLowerCase()
      consType = window.global.idents("ConsultationType")
      switch @opts.kvm.consType()
        when consType.oper
          ops = for u in @ops when not q or u.label.toLowerCase().indexOf(q) != -1
              id: u.id
              label: u.label
              html: u.label
          @vals = ops
          cb @vals.map((u) -> u.html)
        when consType.mech
          mechs = for u in @mechs when not q or u.label.toLowerCase().indexOf(q) != -1
            id: u.id
            label: u.label
            html: "<span><i class='glyphicon glyphicon-wrench'></i>&nbsp;#{u.label}</span>"
          techs = for u in @techs when not q or u.label.toLowerCase().indexOf(q) != -1
            id: u.id
            label: u.label
            html: "<span><i class='glyphicon glyphicon-lamp'></i>&nbsp;#{u.label}</span>"
          @vals = mechs.concat(techs)
          cb @vals.map((u) -> u.html)

    id2val: (i) -> @vals[i].id
    getVal: (l) -> window.global.dictionaries.users.byLabel[l]?.id
    getLab: (v) -> window.global.dictionaries.users.byId[v]?.label

  dict: ConsultantDict
  name: 'ConsultantDict'
