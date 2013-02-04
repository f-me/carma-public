define [ "hooks/common"
       , "hooks/case"
       , "hooks/services"
       , "hooks/partner"
       ],
       (c, k, s, p) ->
  model:
      "*"    : [c.stdElCb]
  observable:
      "*"    : [ c.regexpKbHook
               , c.dictionaryKbHook
               , c.dictManyHook
               , c.filesKbHook
               , c.dateTimeHook
               ]
      "case" : [k.descsKbHook, c.eventsHistoryKbHook]
      "tarifOption": [c.tarifOptNameDef]
      "partner": [p.bindRemoveService, p.serviceRepeat]
      "partner_service": [p.bindTitleServiceName, p.partnerServiceRepeat]
      "cost_serviceTarifOption": [c.tarifOptNameDef]
      "rent"  : [s.partnerOptsHook, s.srvOptUpd, s.costsMark]
      "tech"  : [s.partnerOptsHook, s.srvOptUpd, s.costsMark]
      "taxi"  : [s.partnerOptsHook, s.srvOptUpd, s.costsMark]
      "sober" : [s.partnerOptsHook, s.srvOptUpd]
      "hotel" : [s.partnerOptsHook, s.srvOptUpd, s.costsMark]
      "towage": [s.partnerOptsHook, s.srvOptUpd, s.costsMark, c.distHook]
