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
    (closePrevious InCase
     [AType.tellMeMore, AType.callMeMaybe]
     AResult.communicated *>
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
    (insteadOf Case.caseStatus (const CS.mobileOrder)
     (proceed [AType.callMeMaybe]))


cancel :: Entry
cancel =
    Entry
    (onField Service.status (const SS.canceled) $
     switch
      [ ( serviceField status == const SS.creating ||
          (serviceField status == const SS.backoffice &&
           assigneeOfLast InService
           [AType.orderService, AType.orderServiceAnalyst]
           [noResult] == nobody) ||
          (serviceField status == const SS.makerApproval &&
           assigneeOfLast InService [AType.makerApproval]
           [noResult] == nobody)
        , setServiceField falseCall (const FS.nobill) *>
          closePrevious InService
          [AType.orderService, AType.orderServiceAnalyst, AType.makerApproval]
          AResult.clientCanceledService *>
          closePrevious InCase
          [AType.tellMeMore, AType.callMeMaybe]
          AResult.okButNoService *>
          sendMail PSA *>
          finish
        )
      , ( serviceField status == const SS.backoffice &&
          currentUser ==
          assigneeOfLast InService
          [AType.orderService, AType.orderServiceAnalyst]
          [noResult]
        , closePrevious InService
          [AType.orderService, AType.orderServiceAnalyst]
          AResult.clientCanceledService *>
          sendMail PSA *>
          finish
        )
      ] $
      closePrevious InService
      [ AType.orderService, AType.orderServiceAnalyst
      , AType.tellClient, AType.makerApproval
      , AType.checkStatus, AType.checkEndOfService
      ]
      AResult.clientCanceledService *>
      sendMail PSA *>
      proceed [AType.cancelService]
    )


recallClient :: Entry
recallClient =
    Entry
    (insteadOf Service.status (const SS.recallClient)
     (proceed [AType.checkStatus]))


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
    (const bo_order)
    (ite (previousAction == const AType.needPartner ||
          userField Usermeta.isJack)
     currentUser
     (assigneeOfLast InCase
      [AType.tellMeMore, AType.callMeMaybe] [just AResult.communicated])
    )
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` req (serviceField times_expectedServiceStart)
     in
       ite (t > n) t ((5 * minutes) `since` now)
    )
    [ (AResult.serviceOrdered,
       sendSMS SMS.order *>
       sendMail PSA *>
       setServiceStatus SS.ordered *>
       proceed [AType.tellClient, AType.addBill])
    , (AResult.serviceOrderedSMS,
       sendSMS SMS.order *>
       sendMail PSA *>
       setServiceStatus SS.ordered *>
       proceed [AType.checkStatus, AType.addBill])
    , (AResult.needPartner,
       sendSMS SMS.parguy *>
       setServiceStatus SS.needPartner *>
       proceed [AType.needPartner])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


orderServiceAnalyst :: Action
orderServiceAnalyst =
    Action
    AType.orderServiceAnalyst
    (const bo_secondary)
    nobody
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` req (serviceField times_expectedServiceStart)
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
           ]
         , setServiceStatus SS.ordered *>
           proceed [AType.checkStatus, AType.addBill])
       ]
       (setServiceStatus SS.ordered *>
        proceed [AType.closeCase, AType.addBill]))
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


tellClient :: Action
tellClient =
    Action
    AType.tellClient
    (const bo_control)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.clientOk, proceed [AType.checkStatus])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


checkStatus :: Action
checkStatus =
    Action
    AType.checkStatus
    (const bo_control)
    nobody
    ((5 * minutes) `since` req (serviceField times_expectedServiceStart))
    [ (AResult.serviceInProgress,
       setServiceStatus SS.inProgress *> proceed [AType.checkEndOfService])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


needPartner :: Action
needPartner =
    Action
    AType.needPartner
    (const bo_order)
    nobody
    ((15 * minutes) `since` now)
    [ (AResult.partnerFound,
       setServiceStatus SS.order *> proceed [AType.orderService])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]

checkEndOfService :: Action
checkEndOfService =
    Action
    AType.checkEndOfService
    (const bo_control)
    nobody
    ((5 * minutes) `since` req (serviceField times_expectedServiceEnd))
    [ (AResult.serviceDone,
       sendSMS SMS.complete *>
       sendMail Dealer *>
       setServiceStatus SS.ok *>
       ite (caseField Case.program `oneOf`
            [Program.peugeot, Program.citroen, Program.vw])
       (proceed [AType.closeCase, AType.getDealerInfo])
       (proceed [AType.closeCase]))
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


closeCase :: Action
closeCase =
    Action
    AType.closeCase
    (const Role.head)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.caseClosed, setServiceStatus SS.closed *> finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


getDealerInfo :: Action
getDealerInfo =
    Action
    AType.getDealerInfo
    (const bo_dealer)
    nobody
    (ite
     ((serviceField svcType == const ST.rent) &&
      caseField Case.program `oneOf` [Program.peugeot, Program.citroen])
     ((5 * minutes) `since` req (serviceField times_factServiceEnd))
     ((14 * days) `since` req (serviceField times_factServiceEnd)))
    [ (AResult.gotInfo, sendMail PSA *> finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


cancelService :: Action
cancelService =
    Action
    AType.cancelService
    (const bo_control)
    nobody
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
    , (AResult.supervisorClosed, finish)
    ]

makerApproval :: Action
makerApproval =
    Action
    AType.makerApproval
    (const bo_control)
    nobody
    ((1 * minutes) `since` now)
    [ (AResult.makerApproved,
       setServiceStatus SS.order *> proceed [AType.orderService])
    , (AResult.makerDeclined, proceed [AType.tellMakerDeclined])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


tellMakerDeclined :: Action
tellMakerDeclined =
    Action
    AType.tellMakerDeclined
    (const bo_control)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.clientNotified,
       setServiceStatus SS.closed *> finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


addBill :: Action
addBill =
    Action
    AType.addBill
    (const bo_bill)
    nobody
    ((14 * days) `since` now)
    [ (AResult.billAttached, proceed [AType.headCheck])
    , (AResult.returnToBack, proceed [AType.billmanNeedInfo])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


billmanNeedInfo :: Action
billmanNeedInfo =
    Action
    AType.billmanNeedInfo
    (const bo_qa)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.returnToBillman, proceed [AType.addBill])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


headCheck :: Action
headCheck =
    Action
    AType.headCheck
    (const Role.head)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.confirmedFinal, proceed [AType.analystCheck])
    , (AResult.confirmedWODirector, proceed [AType.accountCheck])
    , (AResult.confirmedHead, proceed [AType.directorCheck])
    , (AResult.returnToBillman, proceed [AType.addBill])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


directorCheck :: Action
directorCheck =
    Action
    AType.directorCheck
    (const bo_director)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.directorToHead, proceed [AType.headCheck])
    , (AResult.confirmedDirector, proceed [AType.accountCheck])
    , (AResult.confirmedFinal, proceed [AType.analystCheck])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


accountCheck :: Action
accountCheck =
    Action
    AType.accountCheck
    (const bo_account)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.accountToDirector, proceed [AType.directorCheck])
    , (AResult.confirmedAccount, proceed [AType.analystCheck])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


analystCheck :: Action
analystCheck =
    Action
    AType.analystCheck
    (const bo_analyst)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.confirmedAnalyst, finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


complaintResolution :: Action
complaintResolution =
    Action
    AType.complaintResolution
    (const bo_qa)
    nobody
    ((1 * minutes) `since` now)
    [ (AResult.complaintManaged, finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


tellMeMore :: Action
tellMeMore =
    Action
    AType.tellMeMore
    (const bo_order)
    nobody
    ((1 * minutes) `since` now)
    [ (AResult.communicated,
       setCaseField caseStatus (const CS.back) *> finish)
    , (AResult.okButNoService,
       setCaseField caseStatus (const CS.back) *> finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


callMeMaybe :: Action
callMeMaybe =
    Action
    AType.callMeMaybe
    (const bo_order)
    nobody
    ((1 * minutes) `since` now)
    [ (AResult.communicated, finish)
    , (AResult.okButNoService, finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


carmaBackoffice :: BackofficeSpec
carmaBackoffice =
    ( [ toBack
      , needInfo
      , needMakerApproval
      , mobileOrder
      , recallClient
      , cancel
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
