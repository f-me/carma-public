{-# LANGUAGE TypeOperators,
             ScopedTypeVariables
 #-}
module Snaplet.Search.Contract where

import           Data.String (fromString)

import           Data.Text (Text)
import qualified Data.Text             as T

import           Text.Printf

import           Database.PostgreSQL.Simple as PG

import           Snap (gets)
import           Snap.Snaplet
import           Snap.Snaplet.Auth

import           Data.Model.Patch (Patch)
import           Data.Model.Sql
import           Data.Model.Types

import           Carma.Model.Contract
import qualified Carma.Model.Usermeta as Usermeta

import           Snaplet.Auth.PGUsers
import           Snaplet.DbLayer

import           Snaplet.Search.Types
import           Snaplet.Search.Utils

contractSearch :: SearchHandler b
              (Either String
               (SearchResult
                (Patch Contract :. ())))
contractSearch = defaultSearch contractSearchParams mkQuery

mkQuery :: forall t.MkSelect t => t -> Text -> Int -> Int -> String -> Query
mkQuery _ predicate lim offset ord
  = fromString $ printf
      (  "    select %s"
      ++ "     from \"Contract\""
      ++ "     where (%s) %s limit %i offset %i;"
      )
      (T.unpack $ mkSel (undefined :: t))
      (T.unpack predicate) ord lim offset

-- | Identical to 'contractSearch', but yields only contracts created
-- by current user if isDealer flag is set.
portalSearch :: SearchHandler b
              (Either String
               (SearchResult
                (Patch Contract :. ())))
portalSearch = do
  u <- with auth currentUser
  case u of
    Just au -> do
        let withDb = (gets db >>=) . flip withTop
        Just (ui, _) <- withDb $ userMetaPG au
        [Only isDealer :. ()] <-
            withDb $
            selectDb (Usermeta.isDealer :. Usermeta.ident `eq` Ident ui)
        let commPred = printf " AND committer = %i" ui
            mkQuery' s p = mkQuery s (if isDealer
                                      then T.append p $ T.pack commPred
                                      else p)
        defaultSearch contractSearchParams mkQuery'
    Nothing -> return $ Left "No user"
