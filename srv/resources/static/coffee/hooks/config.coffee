define [ "hooks/common"
       , "hooks/case"
       , "hooks/services"
       , "hooks/partner"
       , "hooks/actions"
       ],
       (c, k, s, p, a) ->
  model:
      "*"    : [c.stdElCb]
  observable:
      "*"    : [ c.regexpKbHook
               , c.dictionaryKbHook
               , c.dictManyHook
               , c.filesKbHook
               , c.dateTimeHook
               ]
      "action": [a.nameLocal]
      "averageCommissioner": [s.bindPartnerCancelDialog]
      "bank": [s.bindPartnerCancelDialog]
      "case" : [k.descsKbHook, k.eventsHistoryKbHook, k.cityStatsHook]
      "consultation": [s.bindPartnerCancelDialog]
      "continue": [s.bindPartnerCancelDialog]
      "cost_serviceTarifOption": [c.tarifOptNameDef]
      "deliverClient": [s.bindPartnerCancelDialog]
      "hotel" : [s.partnerOptsHook, s.srvOptUpd, s.costsMark, s.bindPartnerCancelDialog]
      "insurance": [s.bindPartnerCancelDialog]
      "ken": [s.bindPartnerCancelDialog]
      "partner": [p.bindRemoveService, p.serviceRepeat]
      "partner_service": [p.bindTitleServiceName, p.partnerServiceRepeat]
      "rent"  : [s.partnerOptsHook, s.srvOptUpd, s.costsMark, s.bindPartnerCancelDialog]
      "sober" : [s.partnerOptsHook, s.srvOptUpd, s.bindPartnerCancelDialog]
      "tarifOption": [c.tarifOptNameDef]
      "taxi"  : [s.partnerOptsHook, s.srvOptUpd, s.costsMark, s.bindPartnerCancelDialog]
      "tech"  : [s.partnerOptsHook, s.srvOptUpd, s.costsMark, s.bindPartnerCancelDialog]
      "tech1": [s.bindPartnerCancelDialog]
      "tickets": [s.bindPartnerCancelDialog]
      # "towage": [s.partnerOptsHook, s.srvOptUpd, s.costsMark, c.distHook, s.bindPartnerCancelDialog]
      # FIXME: remove dist hook
      "towage": [s.partnerOptsHook, s.srvOptUpd, s.costsMark, s.bindPartnerCancelDialog]
