{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
module AppHandlers.Screens (getScreens) where

import           Data.List (intersect)
import           Data.Maybe
import           Data.Aeson
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L8

import           Control.Applicative

import           Database.PostgreSQL.Simple.SqlQQ

import           Snap

import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth
import           Snaplet.Auth.Class

import           Application
import           AppHandlers.Util

import           Utils.HttpErrors


type Permissions = [ByteString]
type Screens = [Screen]

data Screen = Sms { name        :: ByteString
                  , permissions :: Permissions
                  }
            | Li  { name        :: ByteString
                  , label       :: ByteString
                  , permissions :: Permissions
                  }
            | Dropdown { name        :: ByteString
                       , label       :: ByteString
                       , permissions :: Permissions
                       , screens     :: Screens
                       }
              deriving Show

instance FromJSON Screen where
  parseJSON (Object v) = do
    t <- v .: "type"
    case t :: String of
      "sms" -> Sms <$> v .: "name" <*> v .: "permissions"
      "li"  -> Li  <$> v .: "name" <*> v .: "label" <*> v .: "permissions"
      "dropdown" -> Dropdown           <$>
                    v .: "name"        <*>
                    v .: "label"       <*>
                    v .: "permissions" <*>
                    v .: "screens"
      _ -> fail $ "unknown screen type: " ++ t
  parseJSON _ = fail "wrong object in screen list"

instance ToJSON Screen where
  toJSON (Sms name p) = object [ "name" .= name
                               , "type" .= B8.pack "sms"
                               , "permissions" .= p]
  toJSON (Li  n l p ) = object [ "name"        .= n
                               , "label"       .= l
                               , "type"        .= B8.pack "li"
                               , "permissions" .= p
                               ]
  toJSON (Dropdown n l p ss) = object  [ "name"        .= n
                                       , "label"       .= l
                                       , "type"        .= B8.pack "dropdown"
                                       , "permissions" .= p
                                       , "screens"     .= ss
                                       ]

chkPerms :: Permissions -> Permissions -> Bool
chkPerms perms roles = not $ null $ intersect perms roles

processScreens :: Screens -> Permissions -> Screens
processScreens ss rls = reverse $ foldl prcScr [] ss
  where
    prcScr acc d
      | chkPerms (permissions d) rls == True = process acc d
      | otherwise                            = acc
    process acc d@(Dropdown _ _ _ scrs) =
      d{ screens = processScreens scrs rls } : acc
    process acc d = d : acc

readScreens :: IO (Either String Screens)
readScreens = eitherDecode <$> L8.readFile "resources/site-config/screens.json"

-- | Select user roles. User roles are converted to
-- internal values since screens.json references roles by values and
-- currently we have no way to extract ident by string on Haskell side
-- (FIXME)
q :: Query
q = [sql|
WITH u AS
(SELECT unnest(roles) AS id FROM usermetatbl WHERE uid = ?)
SELECT DISTINCT s.roles FROM
(SELECT array_agg(value) AS roles FROM u, "Role" r
 WHERE r.id=u.id::integer) s, u;
|]

getScreens :: AppHandler ()
getScreens = do
  s  <- liftIO readScreens
  (Just (UserId uid)) <- (userId . fromJust) <$> withAuth currentUser
  [Only roles] <- with db $ query q (Only uid)
  case s of
    Right s' -> writeJSON $ processScreens s' roles
    Left err -> finishWithError 403 err
