{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module AppHandlers.Screens (getScreens) where

import           Data.List (intersect)
import qualified Data.Vector as V
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.ByteString.Lazy  as L8

import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple.SqlQQ

import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth
import           Snaplet.Auth.Class

import           Application
import           Util (writeJSON)

type Roles = [Text]

data Screen = Screen
  { __name        :: Text
  , s'type        :: Text
  , __label       :: Maybe Text
  , __nameAliases :: Maybe [Text] -- For correct matching current active screen
  , s'screens     :: Maybe [Screen]
  , s'permissions :: [Text]
  }
$(deriveJSON (defaultOptions {fieldLabelModifier = drop 2}) ''Screen)


filterByPermissions :: Roles -> [Screen] -> [Screen]
filterByPermissions roles = concatMap nestedCheck  . filter canAccess
  where
    canAccess = not . null . intersect roles . s'permissions

    nestedCheck :: Screen -> [Screen]
    nestedCheck = \case
      Screen{s'type = "dropdown", s'screens = Nothing} -> []
      s@Screen{s'type = "dropdown", s'screens = Just ss}
        -> case filterByPermissions roles ss of
          [] -> [] -- skip empty dropdowns
          ss' -> [s{s'screens = Just ss'}]
      s -> [s]


readScreens :: IO (Either String [Screen])
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
  Right screens <- liftIO readScreens
  Just (UserId uid) <- (>>= userId) <$> withAuth currentUser
  [Only roles] <- query q (Only uid)
  writeJSON $ filterByPermissions (V.toList roles) screens
