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

      "Action" : [a.actionColor, a.nameLocal, a.suppressScroll]

      "AverageCommissioner" : [ s.serviceColor
                              , s.updateCaseActions
                              , s.buttons
                              , s.openPartnerSearch
                              ]

      "Bank" : [ s.serviceColor
               , s.updateCaseActions
               , s.buttons
               , s.openPartnerSearch
               ]

      "Case" : [ k.descsKbHook
               , k.eventsHistoryKbHook
               , k.cityStatsHook
               , k.regionHook
               , k.vwfakeHook
               , k.carModelInfoHook
               , k.buttons
               , k.hasFiles
               ]

      "Consultation" : [ s.serviceColor
                       , s.updateCaseActions
                       , s.buttons
                       , s.openPartnerSearch
                       ]

      "Continue" : [ s.serviceColor
                   , s.updateCaseActions
                   , s.buttons
                   , s.openPartnerSearch
                   ]

      "DeliverClient" : [ s.serviceColor
                        , s.updateCaseActions
                        , s.buttons
                        , s.openPartnerSearch
                        ]

      "cost_serviceTarifOption" : [c.tarifOptNameDef]


      "Hotel" : [ s.serviceColor
                , s.updateCaseActions
                , s.buttons
                , s.openPartnerSearch
                ]

      "Information" : [ s.serviceColor
                      , s.updateCaseActions
                      , s.buttons
                      , s.openPartnerSearch
                      ]

      "LegalAssistance" : [ s.serviceColor
                          , s.updateCaseActions
                          , s.buttons
                          , s.openPartnerSearch ]

      "Partner" : [ (c.bindRemoveHook 'services')
                  , p.serviceRepeat
                  , p.factAddr
                  ]

      "partner_service" : [ p.bindTitleServiceName
                          , p.partnerServiceRepeat
                          ]

      "Rent" : [ s.serviceColor
               , s.updateCaseActions
               , s.buttons
               , s.openPartnerSearch
               ]

      "SoberDriver" : [ s.serviceColor
                      , s.updateCaseActions
                      , s.buttons
                      , s.openPartnerSearch
                      ]

      "tarifOption" : [c.tarifOptNameDef]

      "Taxi" : [ s.serviceColor
               , s.updateCaseActions
               , s.buttons
               , s.openPartnerSearch
               ]

      "Tech" : [ s.serviceColor
               , s.updateCaseActions
               , s.buttons
               , s.openPartnerSearch
               ]

      "Transportation" : [ s.serviceColor
                         , s.updateCaseActions
                         , s.buttons
                         , s.openPartnerSearch
                         ]

      "DeliverParts" : [ s.serviceColor
                       , s.updateCaseActions
                       , s.buttons
                       , s.openPartnerSearch
                       ]

      "DeliverCar" : [ s.serviceColor
                     , s.updateCaseActions
                     , s.buttons
                     , s.openPartnerSearch
                     ]

      "TechInspect" : [ s.serviceColor
                      , s.updateCaseActions
                      , s.buttons
                      , s.openPartnerSearch ]

      "Tickets" : [ s.serviceColor
                  , s.updateCaseActions
                  , s.buttons
                  , s.openPartnerSearch ]

      "Towage" : [ s.serviceColor
                 , s.updateCaseActions
                 , s.buttons
                 , s.openPartnerSearch
                 ]

      "SubProgram" : [ c.bindRemoveHook 'services' ]
      "Usermeta": [Um.stateStuff]
