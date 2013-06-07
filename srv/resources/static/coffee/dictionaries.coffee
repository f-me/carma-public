define [ "lib/local-dict"
       , "lib/vin-dict"
       ], (ld, vd) ->

  dicts = {}
  for a in arguments when a.dict?
    dicts[a.dict.name] = a.dict

  dicts: dicts