define ["lib/meta-dict", ], (m) ->
  class BoUsersDict extends m.dict
    lookup: (q, cb) ->
      $.getJSON "/boUsers", (users) =>
        @cache = users
        cb(_.pluck users, 'name')

    id2val: (i) -> @cache[i].login

  dict: BoUsersDict
