define [ "lib/local-dict"
       , "lib/vin-dict"
       , "lib/cards-dict"
       , "lib/bo-users-dict"
       , "lib/computed-dict"
       , "lib/dealers-dict"
       , "lib/cardOwner-dict"
       , "lib/region-dict"
       ], ->

  dicts = {}
  for a in arguments when a.dict?
    dicts[a.dict.name] = a.dict

  dicts: dicts
  dictFromMeta: (kvm, meta) ->
    type = meta.dictionaryType
    opts =
      kvm   : kvm
      dict  : meta.dictionaryName
      parent: meta.dictionaryParent
    new dicts[type || 'LocalDict'](opts)
