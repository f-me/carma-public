{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Triggers
  ( runCreateTriggers
  , runUpdateTriggers
  ) where


import           Prelude hiding (until)

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Free (Free)
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader

import qualified Data.Aeson as Aeson
import           Data.Char (isDigit, toUpper)
import           Data.Singletons
import           Data.Dynamic
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Data.Vector as Vector
import           Text.Printf

import           GHC.TypeLits

import           Snap.Core

import           Database.PostgreSQL.Simple.SqlQQ.Alt
import           Database.PostgreSQL.Simple as PG
import           Snap.Snaplet.PostgresqlSimple as SPG (liftPG', execute)

import           WeatherApi (tempC)

import           Carma.Model.Types (HMDiffTime(..))
import           Data.Model as Model
import           Data.Model.Patch as Patch
import           Data.Model.Types

import           Carma.Model.Action (Action)
import qualified Carma.Model.Action as Action
import           Carma.Model.ActionResult (ActionResult)
import qualified Carma.Model.ActionResult as ActionResult
import qualified Carma.Model.ActionType as ActionType
import qualified Carma.Model.BusinessRole as BusinessRole
import           Carma.Model.Call (Call)
import qualified Carma.Model.Call as Call
import           Carma.Model.Case (Case)
import qualified Carma.Model.Case as Case
import qualified Carma.Model.City as City
import qualified Carma.Model.CaseComment as CaseComment
import qualified Carma.Model.CaseStatus as CS
import           Carma.Model.Contract (Contract, WDay(..))
import qualified Carma.Model.Contract as Contract
import qualified Carma.Model.ContractCheckStatus as CCS
import           Carma.Model.Event (EventType(..))
import qualified Carma.Model.CallType as CT

import           Carma.Model.LegacyTypes

import qualified Carma.Model.Role as Role
import qualified Carma.Model.Service as Service
import           Carma.Model.Service (Service)
import qualified Carma.Model.Service.Hotel as Hotel
import qualified Carma.Model.Service.Rent as Rent
import qualified Carma.Model.Service.Taxi as Taxi
import qualified Carma.Model.Service.Towage as Towage
import           Carma.Model.SubProgram as SubProgram hiding (ident)

import qualified Carma.Model.ServiceStatus as SS
import qualified Carma.Model.ServiceType as ST
import qualified Carma.Model.TowType as TowType
import qualified Carma.Model.UrgentServiceReason as USR
import           Carma.Model.Usermeta (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta
import qualified Carma.Model.Diagnostics.Wazzup as Wazzup
import qualified Carma.Model.Partner as Partner
import           Carma.Model.PartnerDelay (PartnerDelay)
import qualified Carma.Model.PartnerDelay.Confirmed as PartnerDelay_Confirmed
import qualified Carma.Model.DiagHistory as DiagHistory
import qualified Carma.Model.DiagSlide as DiagSlide

import qualified Carma.Model.ProcessingConfig as ProcessingConfig

import           Carma.Backoffice (carmaBackoffice, partnerDelayEntries)
import           Carma.Backoffice.DSL (ActionTypeI, Backoffice)
import qualified Carma.Backoffice.DSL as BO
import           Carma.Backoffice.DSL.Types
import           Carma.Backoffice.Graph (startNode)

import           Application (AppHandler)
import           AppHandlers.ActionAssignment (topPriority, leastPriority)

import qualified Triggers.Action.SMS as BOAction (sendSMS)
import qualified Triggers.Action.MailToGenser as BOAction (sendMailToGenser)
import qualified Triggers.Action.MailToPSA as BOAction (sendMailToPSA)
import qualified Triggers.Action.MailToDealer as BOAction (sendMailToDealer)
import           Triggers.DSL as Dsl

import           Util (Priority(..), syslogJSON, (.=))
import           Data.Model.Utils.PostgreSQL.MSqlQQ hiding (parseQuery)
import           AppHandlers.Util (writeJSON)


vinLength :: Int
vinLength = 17

vinChars :: String
vinChars = ['A'..'H'] ++ ['J'..'N'] ++ ('P':['R'..'Z']) ++ ['0'..'9']

isValidVIN :: Text -> Bool
isValidVIN vin = T.length vin == vinLength &&
                 T.all (`elem` vinChars) (T.map toUpper vin)

minValidSince :: WDay
minValidSince = WDay { unWDay = fromGregorian 2009 1 1}


maxStartMileage, minStartMileage :: Int
minStartMileage = 1
maxStartMileage = 1999999


validPhonePrefixes :: [Text]
validPhonePrefixes = ["+7"]

isValidPhone :: Text -> Bool
isValidPhone phone = prefix `elem` validPhonePrefixes &&
                     T.all isDigit number
    where (prefix, number) = T.splitAt 2 phone


validationFailure :: (KnownSymbol name, Aeson.ToJSON v) =>
                    (m -> Field t (FOpt name desc app))
                    -> v
                    -> AppHandler ()
validationFailure field errorMessage = do
  modifyResponse $ setResponseStatus 400 "Validation Failure"
  writeJSON
    $ Aeson.object
        [ "validationFailure" .= True
        , "validationFields"  .= Aeson.object
            [ Model.fieldName field .= errorMessage ]
        ]
  getResponse >>= finishWith

-- TODO: rename
--   - trigOnModel -> onModel :: ModelCtr m c => c -> Free (Dsl m) res
--   - trigOnField -> onField


beforeCreate :: TriggerMap
beforeCreate = Map.unionsWith (++)
  [ trigOnModel ([]::[Call]) $ do
      getCurrentUser >>= modifyPatch . Patch.put Call.callTaker
      modPut Call.callType (Just CT.info)

  , trigOnModel ([]::[Usermeta]) $ do
      Just login <- getPatchField Usermeta.login -- TODO: check if valid?
      -- NB!
      -- Hope that Snap does not cache users and, in case of error during some
      -- further steps of Usermeta, this will be automatically rolled back.
      -- Otherwise we need some kind of finalisers for "Real world actions that
      -- could not be deferred".
      createSnapUser login

  , trigOnModel ([]::[CaseComment.CaseComment]) $
      getCurrentUser >>= modPut CaseComment.author

  , trigOnModel ([]::[Contract.Contract]) $ do
      modPut Contract.isActive True
      getCurrentUser >>= modPut Contract.committer
      getNow >>= modPut Contract.ctime
      -- Set checkPeriod and validUntil from the subprogram. Remember to
      -- update vinnie_queue triggers when changing these!
      s <- getPatchField Contract.subprogram
      case s of
        Just (Just s') -> do
          sp <- dbRead s'
          modPut Contract.make (sp `get'` SubProgram.defaultMake)
          getPatchField Contract.checkPeriod >>= \case
            Nothing -> modPut Contract.checkPeriod
                              (sp `get'` SubProgram.checkPeriod)
            _       -> return ()
        _ -> return ()
      since <- getPatchField Contract.validSince
      until <- getPatchField Contract.validUntil
      case (s, since, until) of
        (Just (Just s'), Just (Just newSince), Nothing) ->
          fillValidUntil s' newSince
        _ -> return ()

  , trigOnModel ([]::[Case]) $ do
      n <- getNow
      getCurrentUser >>= modPut Case.callTaker
      modPut Case.callDate             $ Just n
      modPut Case.caseStatus             CS.front
      modPut Case.contact_contactOwner $ Just on
      getPatchField Case.comment >>=
        \case
          Just (Just wi) -> fillWazzup wi
          _              -> return ()

  , trigOnModel ([]::[Service]) $ do
      n <- getNow
      modPut Service.createTime         $ Just n
      modPut Service.times_expectedServiceStart $
        Just (addUTCTime (1 * BO.hours) n)
      modPut Service.times_factServiceStart $
        Just (addUTCTime (1 * BO.hours) n)
      modPut Service.times_expectedServiceEnd $
        Just (addUTCTime (2 * BO.hours) n)
      modPut Service.times_factServiceEnd $
        Just (addUTCTime (2 * BO.hours) n)
      modPut Service.times_expectedServiceClosure $
        Just (addUTCTime (12 * BO.hours) n)
      modPut Service.times_factServiceClosure $
        Just (addUTCTime (12 * BO.hours) n)

      modPut Service.createTime         $ Just n
      modPut Service.creator =<< getCurrentUser
      modPut Service.owner   =<< getCurrentUser
      modPut Service.payment_overcosted $ Just off
      modPut Service.status               SS.creating
      modPut Service.urgentService      $ Just USR.notUrgent
      modPut Service.warrantyCase       $ Just off

      Just parentCase <- getPatchField Service.parentId
      [[inRushCity]] <- doApp $ liftPG' $ \pg -> uncurry (PG.query pg)
        [sql|
          select array[cs.city, cs.caseAddress_city] && pc.rushJobCities
            from casetbl cs, "ProcessingConfig" pc
            where cs.id = $(parentCase)$
        |]
      modPut Service.rushJob inRushCity

  , trigOnModel ([]::[Hotel.Hotel]) $
      modPut Hotel.providedFor $ Just "0"

  , trigOnModel ([]::[Rent.Rent]) $
      modPut Rent.providedFor $ Just "0"

  , trigOnModel ([]::[Taxi.Taxi]) $ do
      p <- getPatch
      let parId = fromMaybe (error "No parent case") $
                  p `Patch.get` parentField Service.parentId
      c <- dbRead parId
      modPut Taxi.taxiFrom_address $ c `Patch.get'` Case.caseAddress_address

  , trigOnModel ([]::[Towage.Towage]) $ do
      modPut Towage.accident            $ Just off
      modPut Towage.canNeutral          $ Just off
      modPut Towage.manipulatorPossible $ Just off
      modPut Towage.towType             $ Just TowType.dealer
      modPut Towage.towingPointPresent  $ Just off
      modPut Towage.vandalism           $ Just off
      modPut Towage.wheelsBlocked       $ Just 0

  , trigOnModel ([]::[DiagHistory.DiagHistory]) $ do
      modPut DiagHistory.createdBy =<< getCurrentUser
      getPatchField DiagHistory.slideId >>= \case
        Just _  -> return ()
        Nothing -> do
          caseId <- getPatchField DiagHistory.caseId
          [[slideId]] <- doApp $ liftPG' $ \pg -> uncurry (PG.query pg)
            [sql|
              select diagTree
                from "SubProgram" s join casetbl c on (s.id = c.subprogram)
                where c.id = $(caseId)$
            |]
          modPut DiagHistory.slideId slideId
  ]


afterCreate :: TriggerMap
afterCreate = Map.unionsWith (++) $
  [ trigOnModel ([]::[Usermeta]) $ updateSnapUserFromUsermeta >> do
      getPatchField Usermeta.businessRole >>= \case
        Just (Just bRole) -> dbRead bRole
          >>= modPut Usermeta.roles . (`get'` BusinessRole.roles)
        _ -> return ()

    -- Create a self-assigned action for a call
  , trigOnModel ([]::[Call]) $ do
      ci <- getIdent
      now <- getNow
      us <- getCurrentUser
      let p = Patch.put Action.callId (Just ci) $
              Patch.put Action.aType ActionType.call $
              Patch.put Action.ctime now $
              Patch.put Action.duetime now $
              Patch.put Action.assignTime (Just now) $
              Patch.put Action.openTime (Just now) $
              Patch.put Action.assignedTo (Just us) $
              Patch.put Action.targetGroup Role.call $
              Patch.empty
      aid <- dbCreate p
      logCRUDState Create aid p

  , trigOnModel ([]::[PartnerDelay]) $ do
      delayId <- getIdent
      void $ doApp $ liftPG' $ \pg -> uncurry (PG.execute pg)
        [sql| update servicetbl s
          set
            times_expectedServiceStart
              = times_expectedServiceStart + interval '1m' * p.delayminutes,
            times_expectedServiceStartHistory =
              (select array_to_json(
                array_prepend(
                  to_json(to_char(
                    times_expectedServiceStart at time zone 'UTC',
                    'YYYY-MM-DD HH24:MI:SS')),
                  array(select * from json_array_elements(
                    times_expectedServiceStartHistory
                  ))
               ))::json)
          from "PartnerDelay" p
          where s.id = p.serviceId
            and p.id = $(delayId)$
        |]
      -- FIXME: this should be in Backoffice.partnerDelayEntries
      void $ doApp $ liftPG' $ \pg -> uncurry (PG.execute pg)
        [sql| update actiontbl a
          set duetime = times_expectedServiceStart + interval '5m'
          from "PartnerDelay" p, servicetbl s
          where s.id = p.serviceId
            and p.id = $(delayId)$
            and p.delayConfirmed = $(PartnerDelay_Confirmed.yes)$
            and a.serviceId = p.serviceId
            and a.type = $(ActionType.checkStatus)$
            and a.assignedTo is null
        |]

  ] ++
  map entryToTrigger partnerDelayEntries


beforeUpdate :: TriggerMap
beforeUpdate = Map.unionsWith (++) $
  [ trigOn Usermeta.businessRole $ \case
      Nothing -> return ()
      Just bRole -> do
        roles <- (`get'` BusinessRole.roles) <$> dbRead bRole
        modPut Usermeta.roles roles

  , trigOn Call.redirectTo $ maybe (return ()) $ \newAssignee -> do
      modifyPatch $ Patch.put Call.redirectTo Nothing
      callActs <- getIdent >>= callActionIds
      forM_ callActs $ \aid -> do
        transferAction newAssignee aid
        closeAction ActionResult.transferred aid

  , trigOn Call.endDate $ \case
      Nothing -> return ()
      Just _ -> do
        now <- getNow
        -- Use server time for actual endDate
        modifyPatch $ Patch.put Call.endDate $ Just now
        -- Close all associated call actions
        getIdent >>= callActionIds
                 >>= mapM_ (closeAction ActionResult.callEnded)

  , trigOn ActionType.priority $
      \n -> modPut ActionType.priority $
            if
              | n < topPriority   -> topPriority
              | n > leastPriority -> leastPriority
              | otherwise         -> n

  , trigOn Action.result $ \nr -> do
      ar <- getIdent >>= dbRead
      case ar `Patch.get'` Action.result of
        Just _ -> error "The action already has a result"
        _ -> case nr of
          Nothing -> return ()
          Just _ -> do
            getNow >>= (modifyPatch . Patch.put Action.closeTime . Just)
            getCurrentUser >>=
              (modifyPatch . Patch.put Action.assignedTo . Just)

  , trigOn Action.redirectTo $ maybe (return ()) $ \newAssignee -> do
      getIdent >>= closeAction ActionResult.transferred
      getIdent >>= transferAction newAssignee

  , trigOn Action.assignedTo $ \case
      Nothing -> return ()
      Just _ -> getNow >>=
                (modifyPatch . Patch.put Action.assignTime . Just)

  -- Set validUntil from the subprogram. Remember to update
  -- vinnie_queue triggers when changing this!
  , trigOn Contract.validSince $ \case
      Nothing -> return ()
      Just newSince -> do
        cp <- getIdent >>= dbRead
        let oldSince = cp `get'` Contract.validSince
        do
          s  <- getPatchField Contract.subprogram
          vu <- getPatchField Contract.validUntil
          -- Prefer fields from the patch, but check DB as well
          let  nize (Just Nothing) = Nothing
               nize v              = v
               sub = nize s <|> Just (cp `get'` Contract.subprogram)
               -- We need to check if validUntil field is *missing*
               -- from both patch and DB, thus Just Nothing from DB
               -- is converted to Nothing
               until =
                 nize vu <|> nize (Just $ cp `get'` Contract.validUntil)
          case (sub, until) of
            (Just (Just s'), Nothing) ->
              when (oldSince /= Just newSince) $ fillValidUntil s' newSince
            (_, Just (Just until')) -> do
              when (newSince > until') $ doApp $
                   validationFailure Contract.validSince $
                                     T.concat
                                          [ Model.fieldDesc Contract.validSince
                                          , " не может быть больше "
                                          , Model.fieldDesc Contract.validUntil
                                          ]
              when (newSince < minValidSince) $ doApp $
                   validationFailure Contract.validSince $
                                     T.concat
                                          [ Model.fieldDesc Contract.validSince
                                          , " не может быть меньше "
                                          , T.pack $ show $ unWDay minValidSince
                                          ]
              now <- getNow
              when (UTCTime (Contract.unWDay newSince) 0 > now) $ doApp $
                   validationFailure Contract.validSince $
                                     T.concat
                                          [ Model.fieldDesc Contract.validSince
                                          , " не может быть больше"
                                          , " сегодняшней даты"
                                          ]
            _ -> return ()

  , trigOn Contract.validUntil $ \case
      Nothing -> return ()
      Just newValidUntil -> do
        cp <- getIdent >>= dbRead
        validSince <- getPatchField Contract.validSince
        let nize (Just Nothing) = Nothing
            nize v              = v
            validSince' =
              nize validSince <|> (Just $ cp `get'` Contract.validSince)
        case (validSince', newValidUntil) of
          (Just (Just validSince''), _) ->
              when (validSince'' > newValidUntil) $ doApp $
                   validationFailure Contract.validUntil $
                                     T.concat
                                          [ Model.fieldDesc Contract.validSince
                                          , " не может быть больше "
                                          , Model.fieldDesc Contract.validUntil
                                          ]
          _ -> return ()

  -- Copy some data form prev contract
  , trigOn Contract.vin $ \case
      Just vin | isValidVIN vin -> do
          cId <- getIdent
          prototypeId <- doApp $ liftPG' $ \pg -> uncurry (PG.query pg)
            [sql|
              select c2.id
                from
                  "Contract" c1, "Contract" c2, "SubProgram" s1, "SubProgram" s2
                where c2.dixi
                  and c1.subprogram = s1.id
                  and c2.subprogram = s2.id
                  and s1.parent = s2.parent
                  and c1.id <> c2.id
                  and c1.id = $(cId)$
                  and c2.vin = upper($(vin)$)
                order by c2.ctime desc
                limit 1
            |]
          mapM_ (copyFromContract . head) prototypeId
      Just _ -> doApp $
        validationFailure Contract.vin $
                          T.concat [ "Поле "
                                   , Model.fieldDesc Contract.vin
                                   , " должно содержать "
                                   , T.pack $ show vinLength
                                   , " символов из набора "
                                   , T.pack vinChars
                                   ]
      _ -> return ()

  , trigOn Contract.isActive $ \v -> do
      cId <- getIdent
      uId <- getCurrentUser
      [[canChange]] <- doApp $ liftPG' $ \pg -> uncurry (PG.query pg)
        [sql|
          select (c.dixi = false)
              or (20 = ANY(u.roles))
              or (c.ctime + interval '24 hours' > now()
                and c.subprogram = ANY(u.subprograms))
            from "Contract" c, usermetatbl u
            where c.id = $(cId)$
              and u.id = $(uId)$
        |]
      when canChange $
        modifyPatch $ Patch.put Contract.isActive v
      unless canChange $ do
        currentCtr <- getIdent >>= dbRead
        modifyPatch
          $ Patch.put Contract.isActive
          $ get' currentCtr Contract.isActive

  , trigOn Contract.phone $ \case
      Just phone ->
        if T.length phone == 12
        then when (not $ isValidPhone phone) $ doApp invalidPhone
        else doApp invalidPhone
            where invalidPhone = validationFailure Contract.phone $
                                 T.concat [ Model.fieldDesc Contract.phone
                                          , " должен быть в формате "
                                          , " +7 AAA BBB CC DD"
                                          ]

      _ -> return ()

  , trigOn Contract.startMileage $ \case
      Nothing -> return ()
      Just val ->
        when (val < minStartMileage || val > maxStartMileage) $ doApp $
          validationFailure Contract.startMileage $
                            T.concat [ Model.fieldDesc Contract.startMileage
                                     , " должен быть в диапазоне от "
                                     , T.pack $ show minStartMileage
                                     , " до "
                                     , T.pack $ show maxStartMileage
                                     ]

  , trigOn Case.car_plateNum $ \case
      Nothing -> return ()
      Just val ->
        when (T.length val > 5) $
          modifyPatch
            $ Patch.put Case.car_plateNum
            $ Just $ T.toUpper val

  , trigOn Case.comment $ \case
      Nothing -> return ()
      Just wi -> fillWazzup wi

  , trigOn Case.program $ \_ ->
      modifyPatch $ Patch.put Case.subprogram Nothing

  , trigOn Service.times_expectedServiceStart $ \case
      Nothing -> return ()
      Just tm ->
        modifyPatch $
        Patch.put Service.times_expectedServiceEnd
        (Just $ addUTCTime (1 * BO.hours) tm) .
        Patch.put Service.times_expectedServiceClosure
        (Just $ addUTCTime (11 * BO.hours) tm) .
        Patch.put Service.times_factServiceStart Nothing
  , trigOn Service.times_expectedServiceEnd $ const $
      modifyPatch (Patch.put Service.times_factServiceEnd Nothing)
  , trigOn Service.times_expectedServiceClosure $ const $
      modifyPatch (Patch.put Service.times_factServiceClosure Nothing)

  , trigOn Case.caseAddress_city $ \case
      Nothing -> return ()
      Just city -> do
        cp <- dbRead city
        let cityVal = cp `get'` City.value
        getCityWeather cityVal >>= \case
          Left err -> doApp $ syslogJSON Warning
            "getWeather" ["city" .= cityVal, "error" .= err]
          Right temp -> do
            let temp' = Just $ T.pack $ show $ tempC temp
            modifyPatch $ Patch.put Case.temperature temp'

  , trigOn Case.contract $ \case
      Nothing -> do
        -- Clear all contract-related fields.
        -- NB. we assume they are all nullable
        modifyPatch $ foldl'
          (\fn (C2C _ _ caseFld) -> case Model.fieldName caseFld of
            nm |  nm == Model.fieldName Case.contact_name
               || nm == Model.fieldName Case.contact_phone1
               -> fn
            _ -> Patch.put caseFld Nothing . fn)
          id contractToCase
        modifyPatch $ Patch.put Case.vinChecked Nothing
      Just cid ->
        do
          contract <- dbRead cid
          n <- getNow
          let sinceExceeded =
                case contract `get'` Contract.validSince of
                  Just s  -> n < UTCTime (Contract.unWDay s) 0
                  Nothing -> False
              untilExceeded =
                case contract `get'` Contract.validUntil of
                  Just u  -> n > UTCTime (Contract.unWDay u) 0
                  Nothing -> False
              checkStatus = if sinceExceeded || untilExceeded
                            then CCS.vinExpired
                            else CCS.base
          let Just subProgId = Patch.get' contract Contract.subprogram
          -- The line below is just to convert a FullPatch to a Patch
          let contract' = Patch.delete Contract.ident contract
          copyContractToCase subProgId contract'
          modifyPatch (Patch.put Case.vinChecked $ Just checkStatus)

  , trigOn Service.contractor_partnerId $ \(Just newPartnerId) -> do

    partnerCity <- (`get'` Partner.city) <$> dbRead newPartnerId
    rushCities  <- (`get'` ProcessingConfig.rushJobCities)
                     <$> dbRead ProcessingConfig.main

    -- Set ON/OFF 'rush job' flag for service depending on city of partner.
    -- If partner's city at this moment in 'rush job cities' list
    -- then setting it ON else setting it OFF.
    let x = (`Vector.elem` rushCities) <$> partnerCity
     in modifyPatch $ Patch.put Service.rushJob $ fromMaybe False x

  , trigOn DiagSlide.answers $ \(Aeson.Array answers) -> do
      answers' <- forM (Vector.toList answers) $ \(Aeson.Object ans) ->
        case HM.lookup "nextSlide" ans of
          Just _ -> return $ Aeson.Object ans
          Nothing -> do
            let header = maybe "" (\(Aeson.String s) -> s)
                       $ HM.lookup "header" ans
            [Only newId] <- doApp $ liftPG' $ \pg -> uncurry (PG.query pg)
              [sql|
                insert into "DiagSlide" (header, body)
                  values ($(header)$, '')
                  returning id
              |]
            return $ Aeson.Object
              $ HM.insert "nextSlide" (Aeson.Number $ fromInteger newId) ans
      modPut DiagSlide.answers $ Aeson.Array $ Vector.fromList answers'

  , trigOn DiagHistory.answerIx $ \_ -> do
      modPut DiagHistory.answeredBy =<< Just <$> getCurrentUser
      modPut DiagHistory.answerTime =<< Just <$> getNow

  , actionsToTrigger (snd carmaBackoffice)
  ] ++
  map entryToTrigger (fst carmaBackoffice)


afterUpdate :: TriggerMap
afterUpdate = Map.unionsWith (++)
  [ trigOn Usermeta.delayedState $ const wsMessage
  , trigOn Usermeta.login        $ const updateSnapUserFromUsermeta
  , trigOn Usermeta.password     $ const updateSnapUserFromUsermeta
  , trigOn Usermeta.isActive     $ const updateSnapUserFromUsermeta

  , trigOn Service.contractor_partnerId $ const $ do

      svcId <- getIdent

      -- Updating `contractor_partnerLegacy` field in `Service` table
      void $ doApp $ uncurry SPG.execute
        [msql|
          UPDATE $(T|Service)$ AS svc
            SET $(F|Service.contractor_partnerLegacy)$ = ROW_TO_JSON(js.*)
            FROM
              $(T|Partner)$ AS p,
              JSON_ARRAY_ELEMENTS( p.$(F|Partner.services)$ ) AS s
              JOIN LATERAL ( SELECT
                               s->>'priority1' AS priority1,
                               s->>'priority2' AS priority2,
                               s->>'priority3' AS priority3
                           ) js ON TRUE
            WHERE svc.$(F|Service.ident)$ = $(V|svcId)$
              AND p.$(F|Partner.ident)$
                    = svc.$(F|Service.contractor_partnerId)$
              AND svc.$(F|Service.svcType)$::TEXT = s->>'type'
        |]
  ]


--  - runReadTriggers
--    - ephemeral fields
--      - moves logic from carma-models


-- Utility
----------------------------------------------------------------------

type ModelName = Text
type FieldName = Text
type TriggerMap = Map (ModelName, FieldName) [Dynamic]


-- | This is how we make new trigger
trigOn
  :: forall m name typ desc app res
  . (Model m, KnownSymbol name, Typeable typ)
  => (m -> Field typ (FOpt name desc app)) -- ^ watch this field
  -> (typ -> Free (Dsl m) res)             -- ^ run this if field changed
  -> TriggerMap
trigOn fld fun = Map.singleton (mName, fName) [toDyn fun']
  where
    mName = modelName (modelInfo :: ModelInfo m)
    fName = Model.fieldName fld
    fun'  = getPatchField fld >>= \case
      Nothing  -> error "BUG! We just triggered on this field. It MUST be there."
      Just val -> void $ fun val


trigOnModel
  :: forall m res . Model m
  => [m] -> Free (Dsl m) res -> TriggerMap
trigOnModel _ fun
  = Map.singleton
    (modelName (modelInfo :: ModelInfo m), "") -- dummy field name
    [toDyn $ void fun]


-- | This is how we run triggers on a patch

runCreateTriggers
  :: Model m
  => Patch m -> AppHandler (Either String (IdentI m, Patch m))
runCreateTriggers patch =
  fmap (\st -> (st_ident st, st_patch st))
    <$> runTriggers beforeCreate afterCreate
      (getPatch >>= dbCreate >>= putIdentUnsafe)
      ("" -- pass dummy field name
      : HM.keys (untypedPatch patch) -- just to run PartnerDelay tirggers
      )
      (emptyDslState undefined patch)


runUpdateTriggers
  :: Model m
  => IdentI m -> Patch m
  -> AppHandler (Either String (Patch m))
runUpdateTriggers ident patch =
  fmap st_patch
    <$> runTriggers beforeUpdate afterUpdate
      (getPatch >>= dbUpdate ident >> return ())
      (HM.keys $ untypedPatch patch)
      (emptyDslState ident patch)


runTriggers
  :: forall m . Model m
  => TriggerMap -> TriggerMap -> DslM m () -> [FieldName]
  -> DslState m
  -> AppHandler (Either String (DslState m))
runTriggers before after dbAction fields state = do
  let mInfo = modelInfo :: ModelInfo m

  let matchingTriggers :: Model m' => Text -> TriggerMap -> [DslM m' ()]
      matchingTriggers model trigMap = do
        field <- fields
        Just triggers <- [Map.lookup (model,field) trigMap]
        trigger <- triggers
        return $ fromMaybe -- FIXME: fail early
          (fail $ printf "BUG! while casting tigger (%s,%s)"
            (show model) (show field))
          (fromDynamic trigger)

  let run = runDslM state $ do
        case parentInfo :: ParentInfo m of
          NoParent -> return ()
          ExParent p
            -> inParentContext
            $ sequence_ $ matchingTriggers (modelName p) before
        sequence_ $ matchingTriggers (modelName mInfo) before

        dbAction

        case parentInfo :: ParentInfo m of
          NoParent -> return ()
          ExParent p
            -> inParentContext
            $ sequence_ $ matchingTriggers (modelName p) after
        sequence_ $ matchingTriggers (modelName mInfo) after

  start <- liftIO getCurrentTime
  res <- run
  end <- liftIO getCurrentTime
  syslogJSON Info "runTriggers" ["time" .= show (diffUTCTime end start)]
  return res


-- | Mapping between a contract field and a case field.
data Con2Case = forall t1 t2 n1 d1 n2 d2.
                (Eq t2, Show t2, FieldI t1 n1 d1, FieldI t2 n2 d2) =>
                C2C
                (Contract.Contract -> F (Maybe t1) n1 d1)
                (Maybe t1 -> Maybe t2)
                (Case.Case -> F (Maybe t2) n2 d2)


-- | Mapping between contract and case fields.
contractToCase :: [Con2Case]
contractToCase =
  [ C2C Contract.name id Case.contact_ownerName
  , C2C Contract.phone (fmap Phone) Case.contact_ownerPhone1
  , C2C Contract.generation id Case.car_generation
  , C2C Contract.vin id Case.car_vin
  , C2C Contract.make id Case.car_make
  , C2C Contract.model id Case.car_model
  , C2C Contract.seller id Case.car_seller
  , C2C Contract.plateNum id Case.car_plateNum
  , C2C Contract.makeYear id Case.car_makeYear
  , C2C Contract.color id Case.car_color
  , C2C Contract.buyDate (fmap Contract.unWDay) Case.car_buyDate
  , C2C Contract.firstSaleDate (fmap Contract.unWDay) Case.car_firstSaleDate
  , C2C Contract.lastCheckDealer id Case.car_dealerTO
  , C2C Contract.transmission id Case.car_transmission
  , C2C Contract.engineType id Case.car_engine
  , C2C Contract.engineVolume id Case.car_liters
  , C2C Contract.carClass id Case.car_class
  , C2C Contract.subprogram id Case.subprogram
  ]


copyContractToCase :: IdentI SubProgram -> Patch Contract -> Free (Dsl Case) ()
copyContractToCase subProgId contract = do

  ctrFields <- doApp $ liftPG' $ \pg -> concat <$> uncurry (PG.query pg)
    [sql|
      select contractField
        from "SubProgramContractPermission"
        where showForm
          and parent = $(subProgId)$
    |]

  modifyPatch $ foldl'
    (\fn (C2C ctrFld f caseFld) ->
      let new = f
              $ fromMaybe Nothing -- (join :: Maybe (Maybe a) -> Maybe a)
              $ Patch.get contract ctrFld
          fld = fieldName ctrFld
      in if fld `elem` ctrFields || fld == fieldName Contract.subprogram
          then Patch.put caseFld new . fn
          else fn)
    id contractToCase


copyFromContract :: IdentI Contract -> Free (Dsl Contract) ()
copyFromContract cId = do
  currentCtr <- getIdent >>= dbRead
  protoCtr   <- dbRead cId
  let cp :: FieldI t n d
         => (Contract -> Field (Maybe t) (FOpt n d a))
         -> Patch Contract -> Patch Contract
      cp f = maybe (Patch.put f (get' protoCtr f)) (const id) $ get' currentCtr f

  -- TODO serialize generic fields?
  modifyPatch
    $ cp Contract.name
    . cp Contract.email
    . cp Contract.cardNumber
    . cp Contract.codeWord
    . cp Contract.phone
    . cp Contract.plateNum
    . cp Contract.startMileage
    . cp Contract.make
    . cp Contract.model
    . cp Contract.generation
    . cp Contract.makeYear
    . cp Contract.carClass
    . cp Contract.color
    . cp Contract.transmission
    . cp Contract.engineVolume
    . cp Contract.engineType
    . cp Contract.buyDate
    . cp Contract.firstSaleDate
    . cp Contract.seller
    . cp Contract.registrationReason
    . cp Contract.priceInOrder
    . cp Contract.lastCheckDealer
    . cp Contract.checkPeriod
    . cp Contract.checkType
    . cp Contract.orderNumber
    . cp Contract.managerName
    . cp Contract.comment
    . cp Contract.legalForm


-- | Set @validUntil@ field from a subprogram and a new @validSince@
-- value.
fillValidUntil :: IdentI SubProgram -> WDay -> Free (Dsl Contract) ()
fillValidUntil subprogram newSince = do
  sp <- dbRead subprogram
  let vf = sp `get'` SubProgram.validFor
      vs = unWDay newSince
  case vf of
    Just vf' ->
      modPut Contract.validUntil
      (Just WDay{unWDay = addDays (toInteger vf') vs})
    Nothing -> return ()


-- | Fill @diagnosisN@ fields.
fillWazzup :: IdentI Wazzup.Wazzup -> Free (Dsl Case) ()
fillWazzup wi = do
  wazz <- dbRead wi
  let f :: (FieldI t n d) => (Wazzup.Wazzup -> F t n d) -> t
      f = Patch.get' wazz
      p = Patch.put Case.diagnosis1 (f Wazzup.system) .
          Patch.put Case.diagnosis2 (f Wazzup.part) .
          Patch.put Case.diagnosis3 (f Wazzup.cause) .
          Patch.put Case.diagnosis4 (f Wazzup.suggestion)
  modifyPatch p


-- | transfer action to another user
transferAction :: IdentI Usermeta -> IdentI Action -> Free (Dsl m) ()
transferAction newAssignee actId = do
  act <- dbRead actId
  now <- getNow
  let copy :: (Typeable t, SingI name)
           => (Action -> Field t (FOpt name desc app))
           -> Patch Action -> Patch Action
      copy f = Patch.put f (Patch.get' act f)
  let p = Patch.put Action.ctime now
        $ Patch.put Action.assignTime (Just now)
        $ Patch.put Action.duetime (addUTCTime (1 * BO.minutes) now)
        $ Patch.put Action.assignedTo (Just newAssignee)
        $ Patch.put Action.parent (Just actId)
        $ copy Action.aType
        $ copy Action.targetGroup
        $ copy Action.serviceId
        $ copy Action.caseId
        $ copy Action.callId
        $ Patch.empty
  aid <- dbCreate p
  void $ logCRUDState Create aid p


closeAction :: IdentI ActionResult -> IdentI Action -> Free (Dsl m) ()
closeAction res actId = do
  now <- getNow
  us <- getCurrentUser
  let p = Patch.put Action.closeTime (Just now)
        $ Patch.put Action.assignedTo (Just us)
        $ Patch.put Action.result (Just res)
        $ Patch.empty
  void $ dbUpdate actId p
  void $ logCRUDState Update actId p


-- | Change a field in the patch.
modPut :: (KnownSymbol name, Typeable typ) =>
          (m -> Field typ (FOpt name desc app))
       -> typ
       -> Free (Dsl m) ()
modPut acc val = modifyPatch $ Patch.put acc val


haskellBinary :: (HaskellType t1 -> HaskellType t2 -> HaskellType t)
              -- ^ Non-lifted binary function.
              -> HaskellE t1
              -> HaskellE t2
              -> HaskellE t
haskellBinary fun a b = HaskellE $ fun <$> toHaskell a <*> toHaskell b


-- | Haskell embedding for Backoffice DSL.
newtype HaskellE t = HaskellE { toHaskell :: Reader HCtx (HaskellType t) }
  deriving Typeable


instance Backoffice HaskellE where
  now = HaskellE $ asks Triggers.now
  justNow = HaskellE $ Just <$> asks Triggers.now

  since nd t = HaskellE $ addUTCTime nd <$> toHaskell t

  nobody = nothing

  currentUser = HaskellE $ Just <$> toHaskell (BO.userField Usermeta.ident)

  assigneeOfLast scope types res =
    HaskellE $ do
      res' <- mapM toHaskell res
      ids <- filteredActions scope types (res' :: [Maybe BO.ActionResultI])
      case ids of
        (l:_) -> return $ l `get'` Action.assignedTo
        []    -> return Nothing

  noResult = nothing

  previousAction =
    HaskellE $ fromMaybe (Ident $ fst startNode) <$> asks prevAction

  userField acc = HaskellE $ asks (flip get' acc . user)

  serviceField acc =
    HaskellE $ asks (flip get' acc . fromMaybe (error "No service") . service)

  caseField acc = HaskellE $ asks (flip get' acc . kase)

  const = HaskellE . return

  just    = HaskellE . return . Just
  justTxt = HaskellE . return . Just

  isNotNull = HaskellE . fmap isJust . toHaskell

  req v =
    HaskellE $ fromMaybe (error "Required value not set") <$> toHaskell v

  oneOf e lst =
    HaskellE $ flip elem lst <$> toHaskell e

  switch branches ow =
    HaskellE $
    case branches of
      ((c, br):bs) ->
        toHaskell c >>=
        \case
            True -> toHaskell br
            False -> toHaskell $ BO.switch bs ow
      [] -> toHaskell ow

  not a = HaskellE $ Prelude.not <$> toHaskell a

  (==) = haskellBinary (==)

  (>) = haskellBinary (Prelude.>)

  (&&) = haskellBinary (Prelude.&&)

  (||) = haskellBinary (Prelude.||)

  onField acc target body =
    mkTrigger acc target (`evalHaskell` body)

  insteadOf acc target body =
    mkTrigger acc target $
    \ctx -> do
      -- Reset to old value
      old <- dbRead =<< getIdent
      modifyPatch (Patch.put acc (old `get'` acc))
      evalHaskell ctx body

  setCaseField acc v =
    HaskellE $ do
      ctx <- ask
      let cid = kase ctx `Patch.get'` Case.ident
          val = evalHaskell ctx v
      return $ void $ dbUpdate cid $ put acc val Patch.empty

  setServiceField acc v =
    HaskellE $ do
      ctx <- ask
      sid <- srvId'
      return $ void $ setService sid acc (evalHaskell ctx v)

  sendMail = \case
    Genser -> runLater $ BOAction.sendMailToGenser <$> srvId'
    PSA    -> runLater $ BOAction.sendMailToPSA    <$> srvId'
    Dealer -> runLater $ BOAction.sendMailToDealer <$> srvId'

  sendSMS sendTo tpl =
    runLater $ BOAction.sendSMS tpl <$> srvId' <*> userId' <*> pure sendTo

  when cond act =
    HaskellE $
    toHaskell cond >>=
    \case
      True -> toHaskell act
      False -> return $ return ()

  closePrevious scope types res =
    HaskellE $ do
      ctx <- ask
      let nowFromCtx = Just $ now ctx
      targetActs <- filteredActions scope types [Nothing]
      return $ do
        let currentUser = user ctx `get'` Usermeta.ident
            -- Patch for closing actions
            p   =
              Patch.put Action.result (Just res) $
              -- Set current user as assignee if closing unassigned
              -- action
              --
              -- TODO this is identical to basic Action.result
              -- trigger, which we can't call programmatically
              Patch.put Action.assignedTo (Just currentUser) $
              Patch.put Action.closeTime nowFromCtx Patch.empty
            -- Cause an action-closing event if we're canceling one
            -- of our own actions
            fakeClosing act =
              when (myAction && res == ActionResult.clientCanceledService) $
              void $ logCRUDState Update aid p
                where
                  aid = act `get'` Action.ident
                  myAction = act `get'` Action.assignedTo == Just currentUser
        forM_ targetActs $ \act -> do
          case act `get'` Action.callId of
            Nothing  -> return ()
            Just cId -> void
              -- Don't forget to set Call.endDate if call-action is closed.
              -- Closing call-action for already closed call should never
              -- happen, so we don't afraid to overwrite existing endDate.
              $ dbUpdate cId $ Patch.singleton Call.endDate nowFromCtx
          void $ dbUpdate (act `get'` Action.ident) p
          fakeClosing act

  a *> b =
    HaskellE $ do
      ctx <- ask
      -- Freezing the context at the beginning of the chain we
      -- make all context-changing effects invisible to subsequent
      -- chain operators
      return $ evalHaskell ctx a >> evalHaskell ctx b

  proceed [] = HaskellE $ return $ return ()
  proceed (aT:ts) =
    HaskellE $ do
      ctx <- ask
      return $ do
        this <- getAction
        acts <- caseActions $ kase ctx `get'` Case.ident
        let -- Set current action as a source in the nested
            -- evaluator context
            ctx' = ctx{ prevAction = (`get'` Action.aType) <$> this
                      , actions = acts
                      }
            (e, basePatch) = newActionData ctx' aT
            who = evalHaskell ctx' $ BO.assignment e

        -- Ignore insta-assignment for non-current users if
        -- target user is not Ready
        whoIfReady <-
          case who of
            Just u -> userIsReady u >>= \case
              True -> return who
              False ->
                return $
                if Just currentUser == who
                then Just currentUser
                else Nothing
                where
                  currentUser = user ctx `get'` Usermeta.ident
            Nothing -> return Nothing

        let due = evalHaskell ctx' $ BO.due e
            -- Set assignTime + openTime if a user is picked
            ctime = now ctx
            nowIfWho = maybe Nothing (const $ Just ctime) whoIfReady
            p = Patch.put Action.duetime due $
                Patch.put Action.openTime nowIfWho $
                Patch.put Action.assignedTo whoIfReady $
                Patch.put Action.assignTime nowIfWho $
                Patch.put Action.parent ((`get'` Action.ident) <$> this)
                basePatch
        dbCreate p >> evalHaskell ctx (BO.proceed ts)

  defer =
    HaskellE $ do
      ctx <- ask
      return $ do
        aid <- getIdent
        curPatch <- getPatch
        this <- dbRead aid
        let aT = this `get'` Action.aType
            -- Set current action as a source in the nested
            -- evaluator context
            ctx' = ctx{prevAction = Just aT}
            (_, basePatch) = newActionData ctx' aT

            dbDefer = fromMaybe (error "No deferBy in action") $
                      this `get'` Action.deferBy
            -- If there's no deferBy field in current patch, try
            -- to read it from DB for this action
            HMDiffTime deferBy =
              case curPatch `get` Action.deferBy of
                Just sth -> fromMaybe dbDefer sth
                Nothing -> dbDefer

            -- Truncate everything below seconds, disregard leap
            -- seconds
            deferBy' = realToFrac deferBy
            due = addUTCTime deferBy' (now ctx)
            p = Patch.put Action.duetime due $
                Patch.put Action.parent (Just aid)
                basePatch
        void $ dbCreate p


  withRelatedService (HaskellE f) = HaskellE $ do
    ctx <- ask
    return $ do
      let caseId = kase ctx `get'` Case.ident
      relatedSvcs <- doApp $ liftPG' $ \pg -> uncurry (PG.query pg)
        [sql|
          select id from servicetbl
            where parentId = $(caseId)$
              and (type = $(ST.towage)$ or type = $(ST.tech)$)
              and (status = $(SS.creating)$ or status = $(SS.suspended)$)
        |]
      -- Actually we expect only one related service.
      forM_ relatedSvcs $ \[svcId] -> do
        svcObj <- dbRead svcId
        runReader f (ctx{service = Just svcObj})


-- | Trigger evaluator helper.
--
-- Upon entering the trigger, bootstrap available context for further
-- use in the trigger body. The trigger term itself does not use the
-- context.
mkTrigger :: (Eq (HaskellType t),
              FieldI (HaskellType t) n d, Model m,
              PreContextAccess m) =>
             (m -> F (HaskellType t) n d)
          -> HaskellE t
          -> (HCtx -> Free (Dsl m) ())
          -> HaskellE Trigger
mkTrigger acc target act =
  HaskellE $
  return $
  trigOn acc $ \newVal ->
    -- NB! We are assuming that it is safe to evaluate `target` in empty
    -- context. This may be false if more complex entry conditions are
    -- introduced.
    -- See 9a8dce4d74201a07322d7bb540683e13f8d2f223 for alternative solution.
    when (newVal == evalHaskell emptyContext target)
      $ mkContext Nothing >>= act


-- | Produce a new context for nested Haskell evaluator call.
mkContext :: PreContextAccess m =>
             Maybe ActionTypeI
             -- ^ Previous action type.
          -> Free (Dsl m) HCtx
mkContext act = do
  srv <- getService
  kase' <-
    fromMaybe (error "No case in action when evaluating Backoffice term") <$>
    getKase
  usr <- dbRead =<< getCurrentUser
  acts <- caseActions $ kase' `get'` Case.ident
  t <- getNow
  return HCtx{ kase = kase'
             , service = srv
             , user = usr
             , actions = acts
             , prevAction = act
             , now = t
             }


-- | Graph entry and common data for new actions produced by 'proceed'
-- or 'defer'.
newActionData :: HCtx -> ActionTypeI -> (BO.Action, Patch Action.Action)
newActionData ctx aType = (e, p)
  where
    -- Never breaks for a valid back office
    e = fromMaybe (error "Current action unknown") $
        find ((== aType) . BO.aType) $
        snd carmaBackoffice
    p = Patch.put Action.ctime (now ctx) $
        Patch.put Action.targetGroup (evalHaskell ctx $ BO.targetRole e) $
        Patch.put Action.aType aType $
        Patch.put Action.caseId (Just $ kase ctx `get'` Case.ident) $
        Patch.put Action.serviceId ((`get'` Service.ident) <$> service ctx)
        Patch.empty


nothing :: HaskellE (Maybe t)
nothing = HaskellE $ return Nothing


-- | Obtain service id from the context or fail.
srvId' :: Reader HCtx (IdentI Service)
srvId' = do
  ctx <- ask
  return $
    maybe (error "No service id in context")
          (`get'` Service.ident)
          (service ctx)


userId' :: Reader HCtx (IdentI Usermeta)
userId' = (`get'` Usermeta.ident) . user <$> ask


-- | Run an IO action later in the future
runLater :: Reader HCtx (AppHandler (IO ())) -> HaskellE (Eff m)
runLater = HaskellE . fmap postpone
  where
    -- FIXME Get rid of threadDelay, perform when the database action
    -- returns
    coffebreak = 1500000
    postpone :: AppHandler (IO ()) -> Free (Dsl m) ()
    postpone act =
      Dsl.doApp $
      act >>= \io -> void $ liftIO $ forkIO $ threadDelay coffebreak >> io


-- | Select some actions from the context.
filteredActions :: Scope
                -> [BO.ActionTypeI]
                -- ^ Matching action types.
                -> [Maybe BO.ActionResultI]
                -- ^ Matching action results.
                -> Reader HCtx [Object Action.Action]
filteredActions scope types resList = do
  ctx <- ask
  return $ do
    let sid = (`get'` Service.ident) <$> service ctx
    (`filter` actions ctx) $
      \act ->
        let
          typeOk   = (act `get'` Action.aType) `elem` types
          resultOk = (act `get'` Action.result) `elem` resList
          srvOk    = case scope of
                       InCase -> True
                       -- Filter actions by service if needed. Note
                       -- that _no_ error is raised when called with
                       -- InService from service-less action effect
                       InService -> act `get'` Action.serviceId == sid
        in
          typeOk && resultOk && srvOk


evalHaskell :: HCtx -> HaskellE ty -> HaskellType ty
evalHaskell c t = runReader (toHaskell t) c


-- | Bootstrapping HaskellE context.
emptyContext :: HCtx
emptyContext = error "Empty context accessed (HaskellE interpreter bug)"


-- | Haskell evaluator context.
--
-- Enables data access via pure terms.
data HCtx =
    HCtx { kase       :: Object Case
         , user       :: Object Usermeta
         , service    :: Maybe (Object Service)
         , actions    :: [Object Action.Action]
         , prevAction :: Maybe ActionTypeI
         , now        :: UTCTime
         -- ^ Frozen time.
         }


-- | Convert Backoffice entries to update triggers.
entryToTrigger :: BO.Entry -> Map (ModelName, FieldName) [Dynamic]
entryToTrigger = evalHaskell emptyContext . BO.trigger


-- | Convert Backoffice entries to action result triggers.
actionsToTrigger :: [BO.Action] -> Map (ModelName, FieldName) [Dynamic]
actionsToTrigger acts =
  trigOn Action.result $
  \newVal ->
    case newVal of
      Nothing -> return ()
      Just newRes -> do
        this <- dbRead =<< getIdent
        -- Skip changes for actions of different types
        let aType = get' this Action.aType
        let acts' = filter ((aType ==) . BO.aType) acts
        forM_ acts' $ \a ->
          case lookup newRes (BO.outcomes a) of
            Just o -> do
              hctx <-
                case this `get'` Action.parent of
                  Nothing -> mkContext Nothing
                  Just pid -> do
                    parent <- dbRead pid
                    mkContext $ Just $ parent `get'` Action.aType
              evalHaskell hctx o
            Nothing -> error "Invalid action result"
