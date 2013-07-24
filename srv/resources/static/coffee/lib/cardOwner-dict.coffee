define ["lib/meta-dict", ], (m) ->
  class CardOwnerDict extends m.dict
    find: (q, cb) ->
      return cb({}) if q.length < 5
      $.getJSON "/cardOwnerLookup?q=#{q}", (r) =>
        @found = _.pluck r, 'cardOwner'
        cb @found

    id2val: (i) -> @found[i]

  dict: CardOwnerDict

