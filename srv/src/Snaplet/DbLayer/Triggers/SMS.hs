
module Snaplet.DbLayer.Triggers.SMS where

import Control.Applicative
import Control.Monad.Trans (lift,liftIO)
import Control.Monad
import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import System.Locale

import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import DictionaryCache



render :: Map Text Text -> Text -> Text
render varMap = T.concat . loop
  where
    loop tpl = case T.breakOn "$" tpl of
      (txt, "") -> [txt]
      (txt, tpl') -> case T.breakOn "$" $ T.tail tpl' of
        (expr, "")    -> [txt, evalVar expr]
        (expr, tpl'') -> txt : evalVar expr : loop (T.tail tpl'')

    evalVar v = Map.findWithDefault v v varMap


formatDate :: String -> IO String
formatDate unix = do
  res <- try $ utcToLocalZonedTime $ readTime defaultTimeLocale "%s" unix
  return $ case res :: Either SomeException ZonedTime of
    Right tm -> formatTime defaultTimeLocale "%F %R" tm
    Left  _  -> "неизвестно"



sendSMS :: MonadTrigger m b => ByteString -> ByteString -> m b ()
sendSMS actId tplId = do
  dic <- liftDb $ getDict id

  svcId  <- actId  `get` "parentId"
  caseId <- svcId  `get` "parentId"
  phone  <- caseId `get` "contact_phone1"

  opName  <- T.decodeUtf8 <$> actId `get` "assignedTo"
  cityVal <- T.decodeUtf8 <$> caseId `get` "city"
  program <- T.decodeUtf8 <$> caseId `get` "program"
  let sender = T.encodeUtf8
        $ Map.findWithDefault "RAMC" program
        $ smsTokenVal dic Map.! "program_from_name"

  svcTm <- svcId `get` "times_factServiceStart"
  svcStart <- T.pack <$> liftIO (formatDate $ T.unpack $ T.decodeUtf8 svcTm)

  let varMap = Map.fromList
        [("program_info", (smsTokenVal dic Map.! "program_info") Map.! program)
        ,("program_contact_info",
            (smsTokenVal dic Map.! "program_contact_info") Map.! program)
        ,("case.backoperator_name", Map.findWithDefault opName opName $ user dic)
        ,("case.city", Map.findWithDefault "Город" cityVal $ city dic)
        ,("case.id", (!!1) . T.splitOn ":" $ T.decodeUtf8 caseId)
        ,("service.times_factServiceStart", svcStart)
        ]
  templateText <- T.decodeUtf8 <$> tplId `get` "text"
  let msg = T.encodeUtf8 $ render varMap templateText

  now <- dateNow id
  smsId <- new "sms" $ Map.fromList
    [("ctime", now)
    ,("caseId", caseId)
    ,("svcId", svcId)
    ,("phone", phone)
    ,("template", tplId)
    ,("auto", "true")
    ,("msg", msg)
    ,("sender", sender)
    ]
  Right _ <- redisLPush "smspost" [smsId]
  return ()

updateSMS :: MonadTrigger m b => ObjectId -> m b ()
updateSMS smsId = do
  auto <- get smsId "auto"
  when (auto /= "true") $ do
    caseNum <- get smsId "caseId"
    let caseId = B.append "case:" caseNum

    let add x i y m = do
          yVal <- T.decodeUtf8 <$> get i y
          return $! Map.insert x yVal m
    varMap <- return Map.empty
      >>= return . Map.insert "case.id" (T.decodeUtf8 caseNum)
      >>= add "case.contact_name" caseId "contact_name"
      >>= add "case.caseAddress_address" caseId "caseAddress_address"

    msg <- get smsId "msg"
    _ <- get smsId "template"
    tmp <- T.decodeUtf8 <$> (get smsId "template" >>= (`get` "text"))
    when (msg == "" && tmp /= "") $ do
      let txt = T.encodeUtf8 $ render varMap tmp
      set smsId "msg" txt

    phone <- get smsId "phone"
    when (phone == "") $ do
      get caseId "contact_phone1" >>= set smsId "phone"

    dic <- liftDb $ getDict id
    program <- T.decodeUtf8 <$> caseId `get` "program"
    let sender = T.encodeUtf8
          $ Map.findWithDefault "RAMC" program
          $ smsTokenVal dic Map.! "program_from_name"
    set smsId "sender" sender
