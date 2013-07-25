
module Data.Model.Sql
  (mkSelect
  ,SqlEq(..)
  ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.TypeLits
import Text.Printf

import Data.Model


mkSelect
  :: (SqlProjection proj, SqlPredicate pred)
  => proj -> pred -> (String, PredicateArgs pred)
mkSelect proj pred
  = (projectionSql proj ++ " WHERE " ++ predicateSql pred
    ,predicateArgs pred)

class SqlProjection proj where
  projectionSql :: proj -> String


instance
    (Model model, FromRow (t1,t2)
    ,SingI f1, SingI f2, SingI (TableName model))
    => SqlProjection
      (model -> Field t1 (FOpt f1 d1), model -> Field t2 (FOpt f2 d2))
  where
    projectionSql (f1,f2)
      = printf "SELECT %s, %s FROM %s"
        (fieldName f1) (fieldName f2) (tableName f1)




class ToRow (PredicateArgs pred) => SqlPredicate pred where
  type PredicateArgs pred
  predicateSql  :: pred -> String
  predicateArgs :: pred -> PredicateArgs pred

instance SqlPredicate () where
  type PredicateArgs () = ()
  predicateSql  _ = "true"
  predicateArgs _ = ()

instance (SqlPredicate p1, SqlPredicate p2) => SqlPredicate (p1, p2) where
  type PredicateArgs (p1, p2) = PredicateArgs p1 :. PredicateArgs p2
  predicateSql  (p1,p2) = predicateSql p1 ++ " AND " ++ predicateSql p2
  predicateArgs (p1,p2) = predicateArgs p1 :. predicateArgs p2


data SqlEq model typ name desc = SqlEq
  { eqc_field :: model -> Field typ (FOpt name desc)
  , eqc_val   :: typ
  }

instance (SingI name, ToField typ)
  => SqlPredicate (SqlEq model typ name desc)
  where
    type PredicateArgs (SqlEq model typ name desc) = Only typ
    predicateSql c = fieldName (eqc_field c) ++ " = ?"
    predicateArgs  = Only . eqc_val
