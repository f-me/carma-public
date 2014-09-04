define [ "hooks/common"
       , "hooks/case"
       , "hooks/services"
       , "hooks/partner"
       , "hooks/actions"
       , "hooks/Usermeta"
       ],
       (c, k, s, p, a, Um) ->
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

      "action" : [a.actionColor, a.nameLocal]

      "averageCommissioner" : [ s.serviceColor, s.openPartnerSearch ]

      "bank" : [ s.serviceColor, s.openPartnerSearch ]

      "case" : [ k.descsKbHook
               , k.eventsHistoryKbHook
               , k.cityStatsHook
               , k.regionHook
               , k.vwfakeHook
               , k.carModelInfoHook
               ]

      "consultation" : [ s.serviceColor, s.openPartnerSearch ]

      "continue" : [ s.serviceColor, s.openPartnerSearch ]

      "cost_serviceTarifOption" : [c.tarifOptNameDef]

      "deliverClient" : [ s.serviceColor, s.openPartnerSearch ]

      "hotel" : [ s.serviceColor
                , s.openPartnerSearch
                ]

      "insurance" : [ s.serviceColor, s.openPartnerSearch ]

      "ken" : [ s.serviceColor, s.openPartnerSearch ]

      "Partner" : [ (c.bindRemoveHook 'services')
                  , p.serviceRepeat
                  , p.factAddr
                  ]

      "partner_service" : [ p.bindTitleServiceName
                          , p.partnerServiceRepeat
                          ]

      "rent" : [ s.serviceColor
               , s.openPartnerSearch
               ]

      "sober" : [ s.serviceColor
                , s.openPartnerSearch
                ]

      "tarifOption" : [c.tarifOptNameDef]

      "taxi" : [ s.serviceColor
               , s.openPartnerSearch
               ]

      "tech" : [ s.serviceColor
               , s.openPartnerSearch
               ]

      "transportation" : [ s.serviceColor
                         , s.openPartnerSearch
                         ]

      "deliverParts" : [ s.serviceColor
                       , s.openPartnerSearch
                       ]

      "deliverCar" : [ s.serviceColor
                     , s.openPartnerSearch
                     ]

      "tech1" : [ s.serviceColor, s.openPartnerSearch ]

      "tickets" : [ s.serviceColor, s.openPartnerSearch ]

      "towage" : [ s.serviceColor
                 , s.openPartnerSearch
                 ]

      "SubProgram" : [ c.bindRemoveHook 'services' ]
      "Usermeta": [Um.stateStuff]
