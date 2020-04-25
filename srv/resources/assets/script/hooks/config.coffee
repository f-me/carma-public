c    = require "carma/hooks/common"
k    = require "carma/hooks/case"
s    = require "carma/hooks/services"
p    = require "carma/hooks/partner"
a    = require "carma/hooks/actions"
Um   = require "carma/hooks/Usermeta"
Call = require "carma/hooks/Call"

# Hooks for services that have calculated payments for partners
partnerKpiService = [
  s.serviceColor
  s.partnerWarnedInTimeBtn
  s.updateCaseActions
  s.openPartnerSearch
  s.updatePartnerPayment
]


module.exports =
  model:
      "*"    : [c.stdElCb]
  preinit:
      "Case": [k.timeZoneHook]
  observable:
      "*"    : [ c.regexpKbHook
               , c.dictionaryKbHook
               , c.dictManyHook
               , c.fileKbHook
               , c.dateTimeHook
               , c.listOfTimesHook
               , c.jsonDictObjsHook
               , c.vipPhones
               , c.cityInRushHook
               ]

      "Action" : [a.actionColor, a.nameLocal, a.suppressScroll]

      "AverageCommissioner" : [].concat partnerKpiService

      "Bank" : [ s.serviceColor
               , s.updateCaseActions
               , s.openPartnerSearch
               ]

      "Call" : [ k.programDesc
               , Call.callTypeBtn
               , c.vipPhones
               ]

      "Case" : [ k.descsKbHook
               , k.programDesc
               , k.caseHistory
               , k.caseDiag
               , k.cityStatsHook
               , k.regionHook
               , k.vwfakeHook
               , k.carModelInfoHook
               , k.buttons
               , k.hasFiles
               , k.vip
               , k.contract
               , k.serviceButtons
               , k.locationSharingBtn
               ]

      "Consultation" : [ s.serviceColor
                       , s.updateCaseActions
                       , s.openPartnerSearch
                       , s.consultantOperator
                       ]

      "Continue" : [ s.serviceColor
                   , s.updateCaseActions
                   , s.openPartnerSearch
                   ]

      "DeliverClient" : [ s.serviceColor
                        , s.updateCaseActions
                        , s.openPartnerSearch
                        ]

      "cost_serviceTarifOption" : [c.tarifOptNameDef]


      "Hotel" : [ s.serviceColor
                , s.updateCaseActions
                , s.openPartnerSearch
                ]

      "Information" : [ s.serviceColor
                      , s.updateCaseActions
                      , s.openPartnerSearch
                      ]

      "LegalAssistance" : [ s.serviceColor
                          , s.updateCaseActions
                          , s.openPartnerSearch ]

      "Partner" : [ p.factAddr
                  , p.tarifOptions
                  ]

      "partner_service" : [ p.bindTitleServiceName
                          , p.partnerServiceRepeat
                          ]

      "Rent"        : [].concat partnerKpiService
      "SoberDriver" : [].concat partnerKpiService

      "tarifOption" : [c.tarifOptNameDef]

      "Taxi" : [].concat partnerKpiService
      "Tech" : [].concat partnerKpiService

      "Transportation" : [ s.serviceColor
                         , s.updateCaseActions
                         , s.openPartnerSearch
                         ]

      "DeliverParts" : [ s.serviceColor
                       , s.updateCaseActions
                       , s.openPartnerSearch
                       ]

      "DeliverCar" : [ s.serviceColor
                     , s.updateCaseActions
                     , s.openPartnerSearch
                     ]

      "TechInspect" : [ s.serviceColor
                      , s.updateCaseActions
                      , s.openPartnerSearch ]

      "Tickets" : [ s.serviceColor
                  , s.updateCaseActions
                  , s.openPartnerSearch ]

      "Towage"     : [].concat partnerKpiService
      "BikeTowage" : [].concat partnerKpiService

      "SubProgram" : [ c.bindRemoveHook 'services' ]
      "Usermeta": [Um.stateStuff]
