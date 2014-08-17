{-# LANGUAGE TypeOperators,
             ScopedTypeVariables
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
                (Patch Case :. Maybe (Patch Service) :. Maybe (Patch Towage) :. ())))
caseSearch = defaultSearch
    (caseSearchParams :. serviceSearchParams :. towageSearchParams)
    mkQuery

mkQuery :: forall t.MkSelect t => t -> Text -> Int -> Int -> String -> Query
mkQuery _ pred lim offset ord
  = fromString $ printf
      (  "    select %s "
      ++ "     from casetbl left join servicetbl"
      ++ "       on split_part(servicetbl.parentId, ':', 2)::int = casetbl.id"
      ++ "     left join towagetbl"
      ++ "       on servicetbl.id = towagetbl.id"
      ++ "     where (%s) %s limit %i offset %i;"
      )
      (T.unpack (mkSel (undefined :: t)))
      (T.unpack pred) ord lim offset
