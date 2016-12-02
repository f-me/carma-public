define [ "dictionaries/local-dict"
       , "dictionaries/contracts-dict"
       , "dictionaries/bo-users-dict"
       , "dictionaries/fo-users-dict"
       , "dictionaries/computed-dict"
       , "dictionaries/dealers-dict"
       , "dictionaries/model-dict"
       , "dictionaries/results-dict"
       , "dictionaries/hiddenFields"
       , "dictionaries/consultant-dict"
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
      meta  : meta
    new dicts[type || 'LocalDict'](opts)
