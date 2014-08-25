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

      "Action" : [a.actionColor, a.nameLocal]

      "AverageCommissioner" : [ s.serviceColor, s.openPartnerSearch ]

      "Bank" : [ s.serviceColor, s.openPartnerSearch ]

      "Case" : [ k.descsKbHook
               , k.eventsHistoryKbHook
               , k.cityStatsHook
               , k.regionHook
               , k.vwfakeHook
               , k.carModelInfoHook
               ]

      "Consultation" : [ s.serviceColor, s.openPartnerSearch ]

      "Continue" : [ s.serviceColor, s.openPartnerSearch ]

      "cost_serviceTarifOption" : [c.tarifOptNameDef]

      "DeliverClient" : [ s.serviceColor, s.openPartnerSearch ]

      "Hotel" : [ s.serviceColor
                , s.openPartnerSearch
                ]

      "Insurance" : [ s.serviceColor, s.openPartnerSearch ]

      "LegalAssistance" : [ s.serviceColor, s.openPartnerSearch ]

      "Partner" : [ (c.bindRemoveHook 'services')
                  , p.serviceRepeat
                  , p.factAddr
                  ]

      "partner_service" : [ p.bindTitleServiceName
                          , p.partnerServiceRepeat
                          ]

      "Rent" : [ s.serviceColor
               , s.openPartnerSearch
               ]

      "SoberDriver" : [ s.serviceColor
                , s.openPartnerSearch
                ]

      "tarifOption" : [c.tarifOptNameDef]

      "Taxi" : [ s.serviceColor
               , s.openPartnerSearch
               ]

      "Tech" : [ s.serviceColor
               , s.openPartnerSearch
               ]

      "Transportation" : [ s.serviceColor
                         , s.openPartnerSearch
                         ]

      "DeliverParts" : [ s.serviceColor
                       , s.openPartnerSearch
                       ]

      "DeliverCar" : [ s.serviceColor
                     , s.openPartnerSearch
                     ]

      "TechInspect" : [ s.serviceColor, s.openPartnerSearch ]

      "Tickets" : [ s.serviceColor, s.openPartnerSearch ]

      "Towage" : [ s.serviceColor
                 , s.openPartnerSearch
                 ]

      "SubProgram" : [ c.bindRemoveHook 'services' ]
      "Usermeta": [Um.stateStuff]
