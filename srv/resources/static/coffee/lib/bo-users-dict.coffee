define ["lib/meta-dict", ], (m) ->
  class BoUsersDict extends m.dict
    find: (q, cb) ->
      $.getJSON "/boUsers", (@users) =>
        cb(_.pluck @users, 'name')

    id2val: (i) -> @users[i].login

    getLab: (val) ->
      window.global.dictValueCache.users[val]

    getVal: (lab) ->
      window.global.dictLabelCache.users[lab]
  dict: BoUsersDict
