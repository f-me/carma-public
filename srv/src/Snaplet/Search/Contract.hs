{-# LANGUAGE TypeOperators,
             ScopedTypeVariables
 #-}
module Snaplet.Search.Contract where

import           Data.String (fromString)

import           Data.Text (Text)
import qualified Data.Text             as T

import           Text.Printf

import           Database.PostgreSQL.Simple as PG

import           Data.Model.Patch (Patch)

import           Carma.Model.Contract

import           Snaplet.Search.Types
import           Snaplet.Search.Utils


contractSearch :: SearchHandler b
              (Either String
               (SearchResult
                (Patch Contract :. ())))
contractSearch = defaultSearch contractSearchParams mkQuery

mkQuery :: forall t.MkSelect t => t -> Text -> Int -> Int -> String -> Query
mkQuery _ pred lim offset ord
  = fromString $ printf
      (  "    select %s"
      ++ "     from \"Contract\""
      ++ "     where (%s) %s limit %i offset %i;"
      )
      (T.unpack $ mkSel (undefined :: t))
      (T.unpack pred) ord lim offset
