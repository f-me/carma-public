define [ "lib/local-dict"
       , "lib/vin-dict"
       , "lib/bo-users-dict"
       , "lib/computed-dict"
       , "lib/dealers-dict"
       ], ->

  dicts = {}
  for a in arguments when a.dict?
    dicts[a.dict.name] = a.dict

  dicts: dicts
