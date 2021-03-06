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

module Carma.Backoffice (carmaBackoffice, partnerDelayEntries)

where

import           Prelude hiding ((>), (*>), (==), (||), (&&), const, not)

import qualified Carma.Model.ActionResult as AResult
import qualified Carma.Model.ActionType as AType
import           Carma.Model.Case as Case
import qualified Carma.Model.CaseSource as CO
import qualified Carma.Model.CaseStatus as CS
import qualified Carma.Model.FalseCall as FS
import           Carma.Model.Program as Program
import           Carma.Model.Role as Role
import           Carma.Model.Satisfaction as Satisfaction
import           Carma.Model.Service as Service
import qualified Carma.Model.ServiceStatus as SS
import           Carma.Model.ServiceType as ST
import           Carma.Model.PaymentType as PT
import qualified Carma.Model.SmsTemplate as SMS
import qualified Carma.Model.UrgentServiceReason as USR
import qualified Carma.Model.Usermeta as Usermeta
import qualified Carma.Model.PartnerDelay as PartnerDelay
import qualified Carma.Model.PartnerDelay.Confirmed as PartnerDelay_Confirmed

import Carma.Backoffice.DSL
import Carma.Backoffice.DSL.Types ( Eff
                                  , PreContextAccess
                                  , SvcAccess
                                  , SendSmsTo (..)
                                  )


toBack :: Entry
toBack =
    Entry
    (onField Service.status (const SS.backoffice) toBackAux)

toBackAux
  :: (Backoffice impl, PreContextAccess mdl, SvcAccess mdl)
  => impl (Outcome mdl)
toBackAux =
  closePrevious InCase
                [AType.tellMeMore, AType.callMeMaybe]
                AResult.communicated

    *> when (userField Usermeta.isJack)
            (closePrevious InCase [AType.call] AResult.callEnded)

    *> switch [ ( serviceField svcType
                    `oneOf` [ST.towage, ST.bikeTowage, ST.tech, ST.adjuster]
                , sendSMS SendSmsToCaller SMS.create
                    *> messageToGenser
                    *>
                    switch
                    [ ( serviceField rushJob &&
                        (serviceField urgentService == just USR.notUrgent)
                      , proceed [AType.rushOrder]) ]
                    (proceed [AType.orderService])
                )

              , ( serviceField svcType
                    `oneOf` [ST.ken, ST.medic, ST.consultation]
                , setServiceStatus SS.ok
                    *> proceed [AType.closeCase, AType.addBill]
                )
              ]

              (proceed [AType.orderServiceAnalyst])


messageToGenser :: Backoffice bk => bk (Eff m)
messageToGenser =
    when
    ((caseField Case.program == const Program.genser) &&
     (serviceField Service.svcType == const ST.towage) &&
     (serviceField Service.payType == just PT.ruamc)
    )
    (sendMail Genser) -- FIXME: lift check for Towage.towType from sendMail


messageToPSA :: Backoffice bk => bk (Eff m)
messageToPSA =
    when
    ((caseField Case.program `oneOf` [Program.peugeot, Program.citroen]) &&
     (serviceField Service.svcType `oneOf` [ST.towage, ST.tech, ST.consultation]) &&
     (serviceField Service.payType == just PT.ruamc ||
      serviceField Service.payType == just PT.mixed)
    )
    (sendMail PSA) -- FIXME: lift checks for Towage.techType & consultation.result


messageToDealer :: Backoffice bk => bk (Eff m)
messageToDealer =
    when
    ((caseField Case.program `oneOf` [Program.peugeot, Program.citroen]) &&
     (serviceField Service.svcType == const ST.towage) &&
     (serviceField Service.payType == just PT.ruamc ||
      serviceField Service.payType == just PT.refund ||
      serviceField Service.payType == just PT.mixed)
    )
    (sendMail Dealer)


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
     (setCaseField Case.source (const CO.mobile) *>
      proceed [AType.callMeMaybe]))


mobileAccident :: Entry
mobileAccident =
    Entry
    (insteadOf Case.caseStatus (const CS.mobileAccident)
     (setCaseField Case.source (const CO.mobileAccident) *>
      proceed [AType.accident]))


cancel :: Entry
cancel =
    Entry
    (onField Service.status (const SS.canceled) $
      doCancelService finish (proceed [AType.cancelService]))


doCancelService
  :: (Backoffice impl, PreContextAccess mdl, SvcAccess mdl)
  => impl (Outcome mdl)
  -> impl (Outcome mdl)
  -> impl (Outcome mdl)
doCancelService fin1 fin2 =
     switch
      [ ( serviceField status == const SS.creating ||
          (serviceField status == const SS.backoffice &&
           assigneeOfLast InService
           [AType.orderService, AType.orderServiceAnalyst]
           [noResult] == nobody) ||
          (serviceField status == const SS.makerApproval &&
           assigneeOfLast InService [AType.makerApproval]
           [noResult] == nobody)
        , closePrevious InService
          [AType.orderService, AType.orderServiceAnalyst, AType.makerApproval]
          AResult.clientCanceledService *>
          closePrevious InCase
          [AType.tellMeMore, AType.callMeMaybe]
          AResult.okButNoService *>
          messageToPSA *>
          fin1
        )
      , ( serviceField status == const SS.backoffice &&
          currentUser ==
          assigneeOfLast InService
          [AType.orderService, AType.orderServiceAnalyst]
          [noResult]
        , closePrevious InService
          [AType.orderService, AType.orderServiceAnalyst]
          AResult.clientCanceledService *>
          messageToPSA *>
          fin1
        )
      ] $
      closePrevious InService
      [ AType.orderService, AType.orderServiceAnalyst
      , AType.tellClient, AType.makerApproval
      , AType.checkStatus, AType.checkEndOfService
      ]
      AResult.clientCanceledService *>
      messageToPSA *>
      messageToGenser *>
      fin2


partnerDelayEntries :: [Entry]
partnerDelayEntries =
  [ Entry $ onField
      PartnerDelay.delayConfirmed
      (const PartnerDelay_Confirmed.no)
      cancelServiceDueToPartnerDelay
  , Entry $ onField
      PartnerDelay.delayConfirmed
      (const PartnerDelay_Confirmed.needConfirmation)
      (proceed [AType.confirmPartnerDelay])
  ]


cancelServiceDueToPartnerDelay
  :: (Backoffice impl, PreContextAccess mdl, SvcAccess mdl)
  => impl (Outcome mdl)
cancelServiceDueToPartnerDelay = doCancelService fin fin
  where
    fin =
      setServiceField falseCall (just FS.noService) *>
      setServiceField clientCancelReason (justTxt "Партнёр опоздал") *>
      setServiceStatus SS.canceled *>
      finish

recallClient :: Entry
recallClient =
    Entry
    (insteadOf Service.status (const SS.recallClient)
     (proceed [AType.checkDispatchTime]))


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

activate :: Entry
activate =
    Entry
    (onField Service.status (const SS.creating)
     (setServiceField Service.owner (req currentUser) *>
      finish))

accident :: Action
accident = Action AType.accident
  (const bo_order)
  nobody
  ((5 * minutes) `since` now)

  [ ( AResult.serviceOrdered
    , sendSMS SendSmsToCaller SMS.order
        *> messageToPSA
        *> messageToGenser
        *> setServiceStatus SS.ordered
        *> proceed [AType.tellClient, AType.addBill]
    )

  , ( AResult.serviceOrderedSMS
    , sendSMS SendSmsToCaller SMS.order
        *> messageToPSA
        *> messageToGenser
        *> setServiceStatus SS.ordered
        *> proceed [AType.checkStatus, AType.addBill]
    )

  , ( AResult.needPartner
    , sendSMS SendSmsToCaller SMS.parguy
        *> setServiceStatus SS.needPartner
        *> proceed [AType.needPartner]
    )

  , (AResult.defer, defer)
  , (AResult.supervisorClosed, finish)
  ]


-- | A skeleton for orderService and rushOrder actions.
mkOrderService :: ActionTypeI -> Bool -> Action
mkOrderService at withRushDefer = Action at

  ( ite (serviceField svcType == const ST.adjuster)
        (const bo_orderAvarcom)
        (const bo_order)
  )

  ( ite (  previousAction == const AType.needPartner
        || previousAction == const AType.checkStatus
        || userField Usermeta.isJack
        )
        currentUser
        ( assigneeOfLast InCase
                         [AType.tellMeMore, AType.callMeMaybe]
                         [just AResult.communicated]
        )
  )

  ( let
      n = (1 * minutes) `since` now
      t = (1 * days) `before` req (serviceField times_expectedServiceStart)
    in
      ite (t > n) t ((5 * minutes) `since` now)
  )
  $
  [ ( AResult.serviceOrdered
    , sendSMS SendSmsToCaller SMS.order
        *> notifyPartner
        *> messageToPSA
        *> messageToGenser
        *> setServiceStatus SS.ordered
        *> setServiceField Service.times_expectedDispatch justNow
        *> proceed [AType.tellClient, AType.addBill]
    )

  , ( AResult.serviceOrderedSMS
    , sendSMS SendSmsToCaller SMS.order
        *> notifyPartner
        *> messageToPSA
        *> messageToGenser
        *> setServiceStatus SS.ordered
        *> setServiceField Service.times_expectedDispatch justNow
        *> proceed [AType.checkStatus, AType.addBill]
    )

  , ( AResult.needPartner
    , sendSMS SendSmsToCaller SMS.parguy
        *> setServiceStatus SS.needPartner
        *> proceed [AType.needPartner]
    )

  , (AResult.defer, defer)
  , (AResult.supervisorClosed, finish)
  ] ++
  [(AResult.rushDefer, proceed [AType.rushOrder]) | withRushDefer]


orderService :: Action
orderService = mkOrderService AType.orderService True


rushOrder :: Action
rushOrder = mkOrderService AType.rushOrder False


notifyPartner :: Backoffice bk => bk (Eff m)
notifyPartner = when condition
              $ sendSMS SendSmsToContractorPartner SMS.notifyPartner

  where condition =
          serviceField svcType `oneOf` [ST.towage, ST.tech] &&
          isNotNull (serviceField Service.contractor_partnerId)


orderServiceAnalyst :: Action
orderServiceAnalyst =
    Action
    AType.orderServiceAnalyst
    (ite (serviceField svcType == const ST.tech1)
      (const bo_orderRefs) (const bo_secondary))
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
           messageToGenser *>
           proceed [AType.checkStatus, AType.addBill])
       ]
       (setServiceStatus SS.ordered *>
        messageToGenser *>
        proceed [AType.closeCase, AType.addBill]))
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


tellClient :: Action
tellClient =
    Action
    AType.tellClient
    (switch
      [ (serviceField svcType == const ST.tech1, const bo_controlRefs)
      , (serviceField svcType == const ST.adjuster, const bo_controlAvarcom)
      ]
      (const bo_control))
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.clientOk, proceed [AType.checkStatus])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


checkDispatchTime :: Action
checkDispatchTime =
    Action
    AType.checkDispatchTime
    (const bo_urgent)
    nobody
    ((1 * minutes) `since` now)
    [ (AResult.clientNotified, finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]

checkStatus :: Action
checkStatus =
    Action
    AType.checkStatus
    (switch
      [ (serviceField svcType == const ST.tech1, const bo_controlRefs)
      , (serviceField svcType == const ST.adjuster, const bo_controlAvarcom)
      ]
      (const bo_control))
    nobody
    ((5 * minutes) `since` req (serviceField times_expectedServiceStart))
    [ (AResult.serviceInProgress,
       setServiceStatus SS.inProgress *> proceed [AType.checkEndOfService])
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    , (AResult.needAnotherService,
       setServiceStatus SS.ok *>
        (withRelatedService $ setServiceStatus SS.backoffice *> toBackAux))
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
    (switch
      [ (serviceField svcType == const ST.tech1, const bo_controlRefs)
      , (serviceField svcType == const ST.adjuster, const bo_controlAvarcom)
      ]
      (const bo_control))
    nobody
    ((5 * minutes) `since` req (serviceField times_expectedServiceEnd))
    [ (AResult.serviceDone,
       messageToDealer *>
       messageToGenser *>
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
     ((5 * minutes) `since` req (serviceField times_expectedServiceEnd))
     ((14 * days) `since` req (serviceField times_expectedServiceEnd)))
    [ (AResult.gotInfo, messageToPSA *> finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


cancelService :: Action
cancelService =
    Action
    AType.cancelService
    (switch
      [ (serviceField svcType == const ST.tech1, const bo_controlRefs)
      , (serviceField svcType == const ST.adjuster, const bo_controlAvarcom)
      ]
      (const bo_control))
    nobody
    ((1 * minutes) `since` now)
    [ (AResult.falseCallUnbilled,
       messageToGenser *>
       setServiceStatus SS.canceled *>
       finish)
    , (AResult.falseCallBilled,
       messageToGenser *>
       setServiceStatus SS.canceled *>
       finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]

makerApproval :: Action
makerApproval =
    Action
    AType.makerApproval
    (switch
      [ (serviceField svcType == const ST.tech1, const bo_controlRefs)
      , (serviceField svcType == const ST.adjuster, const bo_controlAvarcom)
      ]
      (const bo_control))
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
    (switch
      [ (serviceField svcType == const ST.tech1, const bo_controlRefs)
      , (serviceField svcType == const ST.adjuster, const bo_controlAvarcom)
      ]
      (const bo_control))
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
    (const bo_info)
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
    (const bo_info)
    nobody
    ((1 * minutes) `since` now)
    [ (AResult.communicated, finish)
    , (AResult.okButNoService, finish)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


confirmPartnerDelay :: Action
confirmPartnerDelay =
    Action
    AType.confirmPartnerDelay
    (const bo_urgent)
    nobody
    ((5 * minutes) `since` now)
    [ (AResult.partnerDelayConfirmed, finish)
    , (AResult.partnerDelayNotConfirmed, cancelServiceDueToPartnerDelay)
    , (AResult.defer, defer)
    , (AResult.supervisorClosed, finish)
    ]


carmaBackoffice :: BackofficeSpec
carmaBackoffice =
    ( [ toBack
      , needInfo
      , needMakerApproval
      , mobileAccident
      , mobileOrder
      , recallClient
      , cancel
      , complaint
      , mistake
      , activate
      ]
    , [ orderService
      , rushOrder
      , accident
      , orderServiceAnalyst
      , tellClient
      , checkStatus
      , checkDispatchTime
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
      , confirmPartnerDelay
      ]
    )
