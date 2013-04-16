module AppHandlers.Screens (getScreens) where

import           Data.Aeson
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L8

import           Control.Applicative

import           Snap

import           Application
import           AppHandlers.Util

type Name  = ByteString
type Label = ByteString
type Permissions = [ByteString]

newtype Screens = Screens [Screen] deriving Show

data Screen = Sms      Name       Permissions
            | Li       Name Label Permissions
            | Dropdown Name Label Permissions Screens
              deriving Show

instance FromJSON Screens where
  parseJSON (Array v) = Screens <$> parseJSON (Array v)
  parseJSON o          = fail $ "can't find screens" ++ show o

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

instance ToJSON Screens where
  toJSON (Screens s) = toJSON s

instance ToJSON Screen where
  toJSON (Sms name p) = object [ "name" .= name, "permissions" .= p]
  toJSON (Li  n l p ) = object [ "name"        .= n
                               , "label"       .= l
                               , "permissions" .= p
                               ]
  toJSON (Dropdown n l p ss) = object  [ "name"        .= n
                                       , "label"       .= l
                                       , "permissions" .= p
                                       , "screens"     .= ss
                                       ]

readScreens :: IO (Either String Screens)
readScreens = eitherDecode <$> L8.readFile "resources/site-config/screens.json"

getScreens :: AppHandler ()
getScreens = do
  s <- liftIO readScreens
  writeJSON s
