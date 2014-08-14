{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

{-|

CaRMa back office definition.

This module provides the concrete specification of our back office.

-}

module Carma.Backoffice (carmaBackoffice)

where

import           Prelude hiding ((>), (==), (||), (&&), const)

import qualified Carma.Model.ActionResult as AResult
import qualified Carma.Model.ActionType as AType
import           Carma.Model.Case as Case
import qualified Carma.Model.CaseStatus as CS
import           Carma.Model.FalseCall as FS
import           Carma.Model.Program as Program
import           Carma.Model.Role as Role
import           Carma.Model.Satisfaction as Satisfaction
import           Carma.Model.Service as Service
import qualified Carma.Model.ServiceStatus as SS
import           Carma.Model.ServiceType as ST
import qualified Carma.Model.SmsTemplate as SMS
import qualified Carma.Model.Usermeta as Usermeta


import Carma.Backoffice.DSL


toBack :: Entry
toBack =
    Entry
    (onField Service.status (const SS.backoffice)
    (([AType.tellMeMore, AType.callMeMaybe] `closeWith` AResult.communicated) *>
     switch
     [ ( serviceField svcType `oneOf` [ST.towage, ST.tech]
       , sendSMS SMS.create *> proceed [AType.orderService]
       )
     , ( serviceField svcType `oneOf` [ST.ken, ST.consultation]
       , sendSMS SMS.complete *>
         setServiceStatus SS.ok *>
         proceed [AType.closeCase, AType.addBill]
       )
     ]
     (proceed [AType.orderServiceAnalyst])
    ))


needMakerApproval :: Entry
needMakerApproval =
    Entry
    (onField Service.status (const SS.makerApproval)
     (proceed [AType.makerApproval]))


needInfo :: Entry
needInfo =
    Entry
    (onField Case.caseStatus (const CS.needInfo)
     (proceed [AType.tellMeMore]))


mobileOrder :: Entry
mobileOrder =
    Entry
    (onField Case.caseStatus (const CS.mobileOrder)
     (proceed [AType.callMeMaybe]))


complaint :: Entry
complaint =
    Entry
    (onField Service.clientSatisfied (just Satisfaction.none)
     (proceed [AType.complaintResolution]))


mistake :: Entry
mistake =
    Entry
    (onField Service.status (const SS.mistake)
     finish)


orderService :: Action
orderService =
    Action
    AType.orderService
    (ite (previousAction == const AType.needPartner ||
          userField Usermeta.isJack)
     (currentUserOr bo_order)
     (role bo_order)
    )
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` (req $ serviceField times_expectedServiceStart)
     in
       ite (t > n) t ((5 * minutes) `since` now)
    )
    [ (AResult.serviceOrdered,
       sendSMS SMS.order *>
       sendPSAMail *>
       setServiceStatus SS.ordered *>
       proceed [AType.tellClient, AType.addBill])
    , (AResult.serviceOrderedSMS,
       sendSMS SMS.order *>
       sendPSAMail *>
       setServiceStatus SS.ordered *>
       proceed [AType.checkStatus, AType.addBill])
    , (AResult.needPartner,
       sendSMS SMS.parguy *>
       setServiceStatus SS.needPartner *>
       proceed [AType.needPartner])
    , (AResult.clientCanceledService,
       sendSMS SMS.cancel *>
       sendPSAMail *>
       setServiceStatus SS.canceled *>
       finish)
    , (AResult.defer, defer)
    ]


orderServiceAnalyst :: Action
orderServiceAnalyst =
    Action
    AType.orderServiceAnalyst
    (role bo_secondary)
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` (req $ serviceField times_expectedServiceStart)
     in
       ite (t > n) t ((5 * minutes) `since` now)
    )
    [ (AResult.serviceOrderedAnalyst,
       switch
       [ ( (serviceField svcType == const ST.rent) &&
           caseField Case.program `oneOf` [Program.peugeot, Program.citroen]
         , setServiceStatus SS.inProgress *>
           proceed [AType.checkEndOfService, AType.addBill]
         )
       , ( serviceField svcType `oneOf`
           [ ST.taxi
           , ST.sober
           , ST.adjuster
           , ST.insurance
           ]
         , setServiceStatus SS.ordered *>
           proceed [AType.checkStatus, AType.addBill])
       ]
       (setServiceStatus SS.ordered *>
        proceed [AType.closeCase, AType.addBill]))
    , (AResult.defer, defer)
    ]


tellClient :: Action
tellClient =
    Action
    AType.tellClient
    (role bo_control)
    ((5 * minutes) `since` now)
    [ (AResult.clientOk, proceed [AType.checkStatus])
    , (AResult.defer, defer)
    ]


checkStatus :: Action
checkStatus =
    Action
    AType.checkStatus
    (role bo_control)
    ((5 * minutes) `since` (req $ serviceField times_expectedServiceStart))
    [ (AResult.serviceInProgress,
       setServiceStatus SS.inProgress *> proceed [AType.checkEndOfService])
    , (AResult.defer, defer)
    ]


needPartner :: Action
needPartner =
    Action
    AType.needPartner
    (currentUserOr bo_order)
    ((15 * minutes) `since` now)
    [ (AResult.partnerFound,
       setServiceStatus SS.order *> proceed [AType.orderService])
    , (AResult.defer, defer)
    ]

checkEndOfService :: Action
checkEndOfService =
    Action
    AType.checkEndOfService
    (role bo_control)
    ((5 * minutes) `since` (req $ serviceField times_expectedServiceEnd))
    [ (AResult.serviceDone,
       sendSMS SMS.complete *>
       sendDealerMail *>
       setServiceStatus SS.ok *>
       ite (caseField Case.program `oneOf`
            [Program.peugeot, Program.citroen, Program.vw])
       (proceed [AType.closeCase, AType.getDealerInfo])
       (proceed [AType.closeCase]))
    , (AResult.defer, defer)
    ]


closeCase :: Action
closeCase =
    Action
    AType.closeCase
    (role Role.head)
    ((5 * minutes) `since` now)
    [ (AResult.caseClosed, setServiceStatus SS.closed *> finish)
    , (AResult.defer, defer)
    ]


getDealerInfo :: Action
getDealerInfo =
    Action
    AType.getDealerInfo
    (role bo_dealer)
    (ite
     ((serviceField svcType == const ST.rent) &&
      caseField Case.program `oneOf` [Program.peugeot, Program.citroen])
     ((5 * minutes) `since` (req $ serviceField times_factServiceEnd))
     ((14 * days) `since` (req $ serviceField times_factServiceEnd)))
    [ (AResult.gotInfo, sendPSAMail *> finish)
    , (AResult.defer, defer)
    ]


cancelService :: Action
cancelService =
    Action
    AType.cancelService
    (role bo_control)
    ((1 * minutes) `since` now)
    [ (AResult.falseCallUnbilled,
       sendSMS SMS.cancel *>
       setServiceStatus SS.canceled *>
       setServiceField Service.falseCall (const FS.nobill) *>
       finish)
    , (AResult.falseCallBilled,
       sendSMS SMS.cancel *>
       setServiceStatus SS.canceled *>
       setServiceField Service.falseCall (const FS.bill) *>
       finish)
    , (AResult.defer, defer)
    ]

makerApproval :: Action
makerApproval =
    Action
    AType.makerApproval
    (role bo_control)
    ((1 * minutes) `since` now)
    [ (AResult.makerApproved, proceed [AType.orderService])
    , (AResult.makerDeclined, proceed [AType.tellMakerDeclined])
    ]


tellMakerDeclined :: Action
tellMakerDeclined =
    Action
    AType.tellMakerDeclined
    (role bo_control)
    ((5 * minutes) `since` now)
    [ (AResult.clientNotified,
       setServiceStatus SS.closed *> finish)
    ]


addBill :: Action
addBill =
    Action
    AType.addBill
    (role bo_bill)
    ((14 * days) `since` now)
    [ (AResult.billAttached, proceed [AType.headCheck])
    , (AResult.returnToBack, proceed [AType.billmanNeedInfo])
    , (AResult.defer, defer)
    ]


billmanNeedInfo :: Action
billmanNeedInfo =
    Action
    AType.billmanNeedInfo
    (role bo_qa)
    ((5 * minutes) `since` now)
    [ (AResult.returnToBillman, proceed [AType.addBill])
    , (AResult.defer, defer)
    ]


headCheck :: Action
headCheck =
    Action
    AType.headCheck
    (role Role.head)
    ((5 * minutes) `since` now)
    [ (AResult.confirmedFinal, proceed [AType.analystCheck])
    , (AResult.confirmedWODirector, proceed [AType.accountCheck])
    , (AResult.confirmedHead, proceed [AType.directorCheck])
    , (AResult.returnToBillman, proceed [AType.addBill])
    , (AResult.defer, defer)
    ]


directorCheck :: Action
directorCheck =
    Action
    AType.directorCheck
    (role bo_director)
    ((5 * minutes) `since` now)
    [ (AResult.directorToHead, proceed [AType.headCheck])
    , (AResult.confirmedDirector, proceed [AType.accountCheck])
    , (AResult.confirmedFinal, proceed [AType.analystCheck])
    , (AResult.defer, defer)
    ]


accountCheck :: Action
accountCheck =
    Action
    AType.accountCheck
    (role bo_account)
    ((5 * minutes) `since` now)
    [ (AResult.accountToDirector, proceed [AType.directorCheck])
    , (AResult.confirmedAccount, proceed [AType.analystCheck])
    , (AResult.defer, defer)
    ]


analystCheck :: Action
analystCheck =
    Action
    AType.analystCheck
    (role bo_analyst)
    ((5 * minutes) `since` now)
    [ (AResult.confirmedAnalyst, finish)
    , (AResult.defer, defer)
    ]


complaintResolution :: Action
complaintResolution =
    Action
    AType.complaintResolution
    (role bo_qa)
    ((1 * minutes) `since` now)
    [ (AResult.complaintManaged, finish)
    , (AResult.defer, defer)
    ]


tellMeMore :: Action
tellMeMore =
    Action
    AType.tellMeMore
    (role bo_order)
    ((1 * minutes) `since` now)
    [ (AResult.communicated, finish)
    , (AResult.okButNoService, finish)
    , (AResult.defer, defer)
    ]


callMeMaybe :: Action
callMeMaybe =
    Action
    AType.callMeMaybe
    (role bo_order)
    ((1 * minutes) `since` now)
    [ (AResult.communicated, finish)
    , (AResult.okButNoService, finish)
    , (AResult.defer, defer)
    ]


carmaBackoffice :: BackofficeSpec
carmaBackoffice =
    ( [ toBack
      , needInfo
      , needMakerApproval
      , mobileOrder
      , complaint
      , mistake
      ]
    , [ orderService
      , orderServiceAnalyst
      , tellClient
      , checkStatus
      , needPartner
      , checkEndOfService
      , closeCase
      , getDealerInfo
      , cancelService
      , makerApproval
      , tellMakerDeclined
      , addBill
      , billmanNeedInfo
      , headCheck
      , directorCheck
      , accountCheck
      , analystCheck
      , complaintResolution
      , callMeMaybe
      , tellMeMore
      ]
    )
