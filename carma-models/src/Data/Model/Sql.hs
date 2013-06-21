
module Data.Model.Sql
  (mkSelectDictQuery
  ,SqlEq(..)
  ,SqlConstraint(..)
  ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.TypeLits

import Data.Model


mkSelectDictQuery
  :: (Model model, SingI (TableName model), SingI name, SqlConstraint ctr)
  => (model -> Field name typ) -> ctr
  -> (String, ValueType ctr)
mkSelectDictQuery f ctr = (sql, sqlVal ctr)
  where
    sql = "SELECT id::text, " ++ fieldName f ++ "::text"
        ++ " FROM " ++ show (tableName f)
        ++ " WHERE " ++ sqlPart ctr


class ToRow (ValueType ctr) => SqlConstraint ctr where
  type ValueType ctr
  sqlPart :: ctr -> String
  sqlVal  :: ctr -> ValueType ctr

instance SqlConstraint () where
  type ValueType () = ()
  sqlPart _ = "true"
  sqlVal  _ = ()

instance (SqlConstraint c1, SqlConstraint c2) => SqlConstraint (c1, c2) where
  type ValueType (c1, c2) = ValueType c1 :. ValueType c2
  sqlPart (c1,c2) = sqlPart c1 ++ " AND " ++ sqlPart c2
  sqlVal  (c1,c2) = sqlVal c1 :. sqlVal c2


data SqlEq name typ model = SqlEq
  { eqc_field :: model -> Field name typ
  , eqc_val   :: typ
  }

instance (SingI name, ToField typ)
  => SqlConstraint (SqlEq name typ model)
  where
    type ValueType (SqlEq name typ model) = Only typ
    sqlPart c = fieldName (eqc_field c) ++ " = ?"
    sqlVal    = Only . eqc_val
