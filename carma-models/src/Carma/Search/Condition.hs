{-# LANGUAGE ScopedTypeVariables, UndecidableInstances, OverlappingInstances #-}

module Carma.Search.Condition where

import           Control.Applicative
import           Control.Monad (liftM2)

import           Data.Monoid

import           Data.List (intercalate)
import           Data.String
import           Data.Typeable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString.Char8 (ByteString)

import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)

import qualified Data.Vector as V
import           Data.Vector(Vector)

import           Database.PostgreSQL.Simple (Connection, formatQuery, Only(..))
import           Database.PostgreSQL.Simple.ToField (ToField(..))

import           GHC.TypeLits

import           Data.Model
import           Carma.Search.Patch

type ConditionsHM m = HashMap
                      Text
                      [(Connection -> Patch m -> IO (Condition m))]

class Conditionable m ctr where
  buildCondition :: Connection
                 -> ConditionsHM m
                 -> Patch m
                 -> ctr
                 -> IO (Condition m)

instance Conditionable m m where
  buildCondition _ _ _ _ = return mempty

instance (SingI n, SingI d, Conditionable m ctr, Model m) =>
         Conditionable m (F t n d -> ctr)
  where
    buildCondition conn cs patch ctr =
      let name  = T.pack $ fromSing (sing :: Sing n)
          applyCs :: Patch m -> [(Connection -> Patch m -> IO (Condition m))]
                     -> IO (Condition m)
          applyCs p conds = mconcat <$> mapM (\f -> f conn p) conds
      in case HM.lookup name cs of
        Just cs' -> liftM2 mappend (applyCs patch cs') $
                                   buildCondition conn cs patch (ctr Field)
        Nothing  -> buildCondition conn cs patch (ctr Field)


--------------------------------------------------------------------------------

data Model m => Condition m = Condition ByteString
                            | EmptyCondition
                            deriving (Show, Eq)

instance Monoid (Condition m) where
  mempty  = EmptyCondition
  mappend (Condition c1)   (Condition c2)  = Condition (c1 <> " AND " <> c2)
  mappend c@(Condition _)  EmptyCondition  = c
  mappend EmptyCondition   c@(Condition _) = c
  mappend EmptyCondition   EmptyCondition  = EmptyCondition

-- when have value in patch then call @f@ with this val, else return
-- @EmptyCondition@
whenHave :: (Model m, SingI n, Typeable t)
            => Patch m
            -> (m -> F t n d)
            -> (t -> IO (Condition m))
            -> IO (Condition m)
whenHave p acc f = maybe (return EmptyCondition) f $ get p acc

class Model m => Fuzzy t m where
  fuzzy :: (m -> t) -> Connection -> Patch m -> IO (Condition m)

instance (Wrapable t, Typeable t, ToField t, SingI n, Model m)
         => Fuzzy (F t n d) m
  where
    fuzzy f c p = whenHave p f $ \v ->
      let fld = fromSing (sing :: Sing n)
          tbl = fromSing (sing :: Sing (TableName m))
          preq = fromString $ tbl <> "." <> fld <> " ilike ?"
      in Condition <$> (formatQuery c preq $ Only $ wrap "%" v)

instance (Wrapable t, Typeable t, ToField t, SingI n, Model m)
         => Fuzzy (F (V.Vector t) n d) m
  where
    fuzzy f c p = whenHave p f $ \v ->
      let fld = fromSing (sing :: Sing n)
          tbl = fromSing (sing :: Sing (TableName m))
          q    = inParen $ intercalate " OR " $
                 replicate (V.length v) (tbl <> "." <> fld <> " ilike ?")
      in Condition <$> (formatQuery c (fromString q) $ map (wrap "%") $ V.toList v)

--------------------------------------------------------------------------------

class Model m => Full f m where
  full  :: (m -> f) -> Connection -> Patch m -> IO (Condition m)

instance (Typeable t, ToField t, SingI n, Model m)
         => Full (F t n d) m
  where
    full acc conn patch = whenHave patch acc $ \v ->
      let fld = fromSing (sing :: Sing n)
          tbl = fromSing (sing :: Sing (TableName m))
          preq = fromString $ tbl <> "." <> fld <> " = ?"
      in Condition <$> (formatQuery conn preq $ Only v)

instance (Typeable t, ToField t, SingI n, Model m)
         => Full (F (Vector t) n d) m
  where
    full acc conn patch = whenHave patch acc $ \v ->
      let fld = fromSing (sing :: Sing n)
          tbl = fromSing (sing :: Sing (TableName m))
          preq = fromString $ tbl <> "." <> fld <> " = ANY (?)"
      in Condition <$> (formatQuery conn preq $ Only v)

--------------------------------------------------------------------------------

class Model m => FuzzyMany f m where
  fuzzyMany  :: (m -> f) -> [String] -> Connection -> Patch m -> IO (Condition m)

instance (Wrapable t, Typeable t, ToField t, SingI n, Model m)
         => FuzzyMany (F t n d) m
  where
    fuzzyMany acc flds conn patch = whenHave patch acc $ \v ->
      let tbl  = fromSing (sing :: Sing (TableName m))
          preq = fromString $ inParen $ intercalate " OR " $
                 map (\f -> tbl <> "." <> f <> " ilike ?") flds
          q    = formatQuery conn preq $
                 map (wrap "%") $
                 replicate (length flds) v
      in Condition <$> q

--------------------------------------------------------------------------------

class IsString t => Wrapable t where
  wrap  :: t -> t -> t
  wrap2 :: t -> t -> t -> t
  wrap a w = wrap2 a w a
  inParen :: t -> t
  inParen s = wrap2 "(" s ")"

instance (IsString t, Monoid t) => Wrapable t where
  wrap2 a what b = a <> what <> b
