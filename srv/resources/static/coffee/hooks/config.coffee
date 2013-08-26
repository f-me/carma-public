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
               , c.fileKbHook
               , c.dateTimeHook
               , c.jsonDictObjsHook
               ]

      "action" : [a.nameLocal]

      "averageCommissioner" : [ s.openPartnerSearch ]

      "bank" : [ s.openPartnerSearch ]

      "case" : [ k.descsKbHook
               , k.eventsHistoryKbHook
               , k.cityStatsHook
               ]

      "consultation" : [ s.openPartnerSearch ]

      "continue" : [ s.openPartnerSearch ]

      "cost_serviceTarifOption" : [c.tarifOptNameDef]

      "deliverClient" : [ s.openPartnerSearch ]

      "hotel" : [ s.partnerOptsHook
                , s.srvOptUpd
                , s.costsMark
                , s.openPartnerSearch
                ]

      "insurance" : [ s.openPartnerSearch ]

      "ken" : [ s.openPartnerSearch ]

      "partner" : [p.bindRemoveService, p.serviceRepeat, p.factAddr]

      "partner_service" : [ p.bindTitleServiceName
                          , p.partnerServiceRepeat
                          ]

      "rent" : [ s.partnerOptsHook
               , s.srvOptUpd
               , s.costsMark
               , s.openPartnerSearch
               ]

      "sober" : [ s.partnerOptsHook
                , s.srvOptUpd
                , s.openPartnerSearch
                ]

      "tarifOption" : [c.tarifOptNameDef]

      "taxi" : [ s.partnerOptsHook
               , s.srvOptUpd
               , s.costsMark
               , s.openPartnerSearch
               ]

      "tech" : [ s.partnerOptsHook
               , s.srvOptUpd
               , s.costsMark
               , s.openPartnerSearch
               ]

      "transportation" : [ s.partnerOptsHook
                         , s.srvOptUpd
                         , s.costsMark
                         , s.openPartnerSearch
                         ]

      "tech1" : [ s.openPartnerSearch ]

      "tickets" : [ s.openPartnerSearch ]

      "towage" : [ s.partnerOptsHook
                 , s.srvOptUpd
                 , s.costsMark
                 , s.openPartnerSearch
                 ]
