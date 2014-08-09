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
import qualified Prelude as P ((>), (==), (||), (&&), const)

import qualified Carma.Model.ActionResult as AResult
import qualified Carma.Model.ActionType as AType
import           Carma.Model.Case.Type as Case
import qualified Carma.Model.CaseStatus as CS
import           Carma.Model.FalseCall as FS
import           Carma.Model.Program as Program
import           Carma.Model.Role as Role
import           Carma.Model.Service as Service
import qualified Carma.Model.ServiceStatus as SS
import           Carma.Model.ServiceType as ST
import qualified Carma.Model.SmsTemplate as SMS
import qualified Carma.Model.Usermeta as Usermeta


import Carma.Backoffice.DSL


toBack :: Entry
toBack =
    Entry
    (Service.status `onServiceField` const SS.backoffice)
    (proceed [AType.orderService])


needInfo :: Entry
needInfo =
    Entry
    (Case.caseStatus `onCaseField` const CS.needInfo)
    (proceed [AType.tellMeMore])


orderService :: Action
orderService =
    Action
    AType.orderService
    (role bo_order)
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` serviceField' times_expectedServiceStart
     in
       switch [(t > n, t)] ((5 * minutes) `since` now)
    )
    [ (AResult.serviceOrdered,
       sendSMS SMS.order *>
       setServiceStatus SS.ordered *>
       proceed [AType.tellClient])
    , (AResult.serviceOrderedSMS,
       sendSMS SMS.order *>
       setServiceStatus SS.ordered *>
       proceed [AType.checkStatus])
    , (AResult.clientCanceledService,
       sendSMS SMS.cancel *>
       setServiceStatus SS.canceled *>
       finish)
    , (AResult.defer, defer)
    ]


tellClient :: Action
tellClient =
    Action
    AType.tellClient
    (role bo_control)
    ((5 * minutes) `since` now)
    [ (AResult.clientOk,
       proceed [AType.checkStatus])
    , (AResult.clientCanceledService,
       proceed [AType.cancelService])
    , (AResult.defer, defer)
    ]


checkStatus :: Action
checkStatus =
    Action
    AType.checkStatus
    (role bo_control)
    ((5 * minutes) `since` serviceField' times_expectedServiceStart)
    [ (AResult.serviceInProgress,
       setServiceStatus SS.inProgress *> proceed [AType.checkEndOfService])
    , (AResult.clientCanceledService,
       proceed [AType.cancelService])
    , (AResult.defer, defer)
    ]


checkEndOfService :: Action
checkEndOfService =
    Action
    AType.checkEndOfService
    (role bo_control)
    ((5 * minutes) `since` serviceField' times_expectedServiceEnd)
    [ (AResult.serviceDone,
       proceed [AType.closeCase])
    , (AResult.clientComplained,
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


cancelService :: Action
cancelService =
    Action
    AType.cancelService
    (role bo_control)
    ((1 * minutes) `since` now)
    [ (AResult.falseCall,
       setServiceStatus SS.canceled *>
       setServiceField Service.falseCall FS.bill *>
       finish)
    , (AResult.defer, defer)
    ]


tellMeMore :: Action
tellMeMore =
    Action
    AType.tellMeMore
    (role bo_order)
    ((1 * minutes) `since` now)
    [ (AResult.serviceOrderedSMS,
       proceed [AType.checkStatus])
    , (AResult.clientCanceledService,
       setServiceStatus SS.canceled *>
       setServiceField Service.falseCall FS.bill *>
       finish)
    , (AResult.couldNotReach,
       proceed [AType.tellClient])
    , (AResult.defer, defer)
    ]


carmaBackoffice :: BackofficeSpec
carmaBackoffice =
    ( [ toBack
      , needInfo
      ]
    , [ orderService
      , tellClient
      , checkStatus
      , checkEndOfService
      , closeCase
      , cancelService
      , tellMeMore
      ]
    )
