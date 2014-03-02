{-# LANGUAGE TypeOperators #-}
module Snaplet.Search.Call where

import           Data.String (fromString)

import           Data.Text (Text)
import qualified Data.Text             as T

import           Text.Printf

import           Database.PostgreSQL.Simple as PG

import           Data.Model.Patch (Patch)

import           Carma.Model.Call

import           Snaplet.Search.Types
import           Snaplet.Search.Utils


callSearch :: SearchHandler b
              (Either String
               (SearchResult
                (Patch Call :. ())))
callSearch = do
  mkSearch (callSearchParams)
    mkQuery $ \conn roles (Only c) ->
      stripRead conn roles $ (parsePatch c) :. ()

mkQuery :: Text -> Int -> Int -> String -> Query
mkQuery pred lim offset ord
  = fromString $ printf
      (  "    select row_to_json(calltbl.*) :: text"
      ++ "     from calltbl"
      ++ "     where (%s) %s limit %i offset %i;"
      )
      (T.unpack pred) ord lim offset
