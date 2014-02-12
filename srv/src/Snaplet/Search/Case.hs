{-# LANGUAGE TypeOperators
 #-}
module Snaplet.Search.Case where

import           Data.String (fromString)

import           Data.Text (Text)
import qualified Data.Text             as T

import           Text.Printf

import           Database.PostgreSQL.Simple as PG

import           Data.Model.Patch (Patch)

import           Carma.Model.Case
import           Carma.Model.Service
import           Carma.Model.Service.Towage

import           Snaplet.Search.Types
import           Snaplet.Search.Utils


caseSearch :: SearchHandler b
              (Either String
               (SearchResult
                (Patch Case :. Patch Service :. Patch Towage :. ())))
caseSearch = do
  mkSearch (caseSearchParams :. serviceSearchParams :. towageSearchParams)
    mkQuery $ \conn roles (c, s, t) -> stripRead conn roles $
      (parsePatch c) :. (parsePatch s) :. (parsePatch t) :. ()

mkQuery :: Text -> Int -> Int -> String -> Query
mkQuery pred lim offset ord
  = fromString $ printf
      (  "    select row_to_json(casetbl.*)    :: text,"
      ++ "           row_to_json(servicetbl.*) :: text,"
      ++ "           row_to_json(towagetbl.*)  :: text"
      ++ "     from casetbl left join servicetbl"
      ++ "       on split_part(servicetbl.parentId, ':', 2)::int = casetbl.id"
      ++ "     left join towagetbl"
      ++ "       on servicetbl.id = towagetbl.id"
      ++ "     where (%s) %s limit %i offset %i;"
      )
      (T.unpack pred) ord lim offset
