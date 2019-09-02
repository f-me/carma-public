{-# LANGUAGE RankNTypes, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE KindSignatures, DataKinds, GADTs, UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings, LambdaCase #-}

-- | Helper to build kinda interpolated relatively safe RAW SQL.
--
-- By \"interpolated" it means that you use values and other dynamic stuff
-- inplace, instead of adding placeholders and providing a value apart in a
-- list.
--
-- See also "Carma.Monad.Esqueleto.MonadRawEsqueleto", you're supposed to use
-- this builder of raw SQL queries inside such a monad.
module Carma.Utils.Persistent.RawSqlQueryConstructor
     ( RawSqlPiece (..)

     , TableAliasToken
     , mkTableAliasToken

     , TableAliasFieldToken
     , mkTableAliasFieldToken

     , buildRawSql

     , RawBasic (..)
     , raw
     , rawSelectAlias
     , rawIntersected
     , rawSeq

     , getTableByItsField
     ) where

import           GHC.TypeLits

import           Data.Proxy
import           Data.Typeable
import           Data.Functor.Identity
import qualified Data.Vector as V
import           Data.Text (Text)
import           Text.InterpolatedString.QM

import           Database.Persist

import           Carma.Utils.Operators
import           Carma.Utils.Cons


------------------------------------------------------------------------------
-- Main types and instances
------------------------------------------------------------------------------


data RawSqlPiece (f :: * -> *)
   = RawPlain Text
   -- ^ A piece of plain SQL text

   | forall model
   . PersistEntity model
   => RawTable (Proxy model)
   -- ^ Table name

   | forall model fieldType
   . PersistEntity model
   => RawField (EntityField model fieldType)
   -- ^ Field name in context of table name

   | forall a
   . (Show a, PersistField a)
   => RawValue a
   -- ^ An interpolated value

   | forall model a
   . (PersistEntity model, PersistField a, Show a)
   => RawFieldValue (EntityField model a) a
   -- ^ A proven table field interpolated value
   --   (proven type of that value by provided field).

   | forall m alias a
   . (PersistEntity m, KnownSymbol alias, Typeable a, PersistField a, Show a)
   => RawTableAliasFieldValue (TableAliasFieldToken m alias a) a
   -- ^ A proven table field interpolated value in context of alias
   --   (proven type of that value by provided field).

   | forall model alias
   . (PersistEntity model, KnownSymbol alias)
   => RawTableAlias (TableAliasToken model alias)
   -- ^ Declare a table name with some alias to it
   --   ("AS" and then alias name, usual for "FROM"-statement).

   | forall model alias fieldType
   . (PersistEntity model, KnownSymbol alias, Typeable fieldType)
   => RawTableAliasField (TableAliasFieldToken model alias fieldType)
   -- ^ Field in context of table alias.

   | forall alias
   . (KnownSymbol alias, Show (f (RawSqlPiece f)), Eq (f (RawSqlPiece f)))
   => RawAliasAs (Proxy alias) (f (RawSqlPiece f))
   -- ^ Declare a table name with some alias to it in @WITH@-statement form
   --   (alias name first then "AS").
   --
   -- @[RawSqlPiece]@ are innards of this @WITH@ alias.

   | forall alias
   . KnownSymbol alias
   => RawAlias (Proxy alias)
   -- ^ Just print alias name.

   | forall alias
   . KnownSymbol alias
   => RawAliasedName (Proxy alias) Text
   -- ^ Declare name which is requested from provided alias.
   --
   -- Useful to call something defined in @WITH@-statement.
   -- To call something from a table please use "RawTableAliasField".

   | (Show (f (RawSqlPiece f)), Eq (f (RawSqlPiece f)))
   => RawWrap (f (RawSqlPiece f))
   -- ^ To wrap something into parenthesis

   | (Show (f (RawSqlPiece f)), Eq (f (RawSqlPiece f)))
   => RawWrap' (Text, Text) (f (RawSqlPiece f))
   -- ^ Like "RawWrap" but with custom open and close wrapping symbols.

data TableAliasToken (model :: *) (alias :: Symbol) = TableAliasToken

newtype TableAliasFieldToken (model :: *) (alias :: Symbol) (fieldType :: *)
      = TableAliasFieldToken (EntityField model fieldType)

instance Eq (f (RawSqlPiece f)) => Eq (RawSqlPiece f) where
  RawPlain x == RawPlain y = x == y
  RawPlain _ == _ = False

  RawTable                x == RawTable              y =
    entityDefByModelProxy x == entityDefByModelProxy y

  RawTable _ == _ = False

  RawField             x == RawField         y =
    entityDefByField x   == entityDefByField y &&
    persistFieldDef    x == persistFieldDef  y

  RawField _ == _ = False

  -- Equality of database value representation
  RawValue         x == RawValue       y =
    toPersistValue x == toPersistValue y

  RawValue _ == _ = False

  -- Equality of database value representation
  RawFieldValue      x y == RawFieldValue   a b =
    persistFieldDef  x   == persistFieldDef a    &&
    toPersistValue     y == toPersistValue    b

  RawFieldValue _ _ == _ = False

  RawTableAliasFieldValue          x y == RawTableAliasFieldValue        a b =
    entityDefByTableAliasToken     x   == entityDefByTableAliasToken     a   &&
    aliasByTableAliasToken         x   == aliasByTableAliasToken         a   &&
    fieldDefByTableAliasFieldToken x   == fieldDefByTableAliasFieldToken a   &&
    toPersistValue                   y == toPersistValue                   b

  RawTableAliasFieldValue _ _ == _ = False

  RawTableAlias                x == RawTableAlias              y =
    entityDefByTableAliasToken x == entityDefByTableAliasToken y &&
    aliasByTableAliasToken     x == aliasByTableAliasToken     y

  RawTableAlias _ == _ = False

  RawTableAliasField               x == RawTableAliasField             y =
    entityDefByTableAliasToken     x == entityDefByTableAliasToken     y &&
    aliasByTableAliasToken         x == aliasByTableAliasToken         y &&
    fieldDefByTableAliasFieldToken x == fieldDefByTableAliasFieldToken y

  RawTableAliasField _ == _ = False

  RawAliasAs  x y == RawAliasAs a b =
    symbolVal x   == symbolVal  a   &&
                y ==              b

  RawAliasAs _ _ == _ = False

  RawAlias x == RawAlias y = symbolVal x == symbolVal y
  RawAlias _ == _ = False

  RawAliasedName x y == RawAliasedName a b =
    symbolVal    x   == symbolVal      a   &&
                   y ==                  b

  RawAliasedName _ _ == _ = False

  RawWrap x == RawWrap y = x == y
  RawWrap _ == _ = False

  RawWrap' x y == RawWrap' a b = x == a && y == b
  RawWrap' _ _ == _ = False

instance Show (f (RawSqlPiece f)) => Show (RawSqlPiece f) where
  show (RawPlain x) = [qm| RawPlain {show x} |]

  show (RawTable x) = go where
    model = entityDefByModelProxy x

    go = [qm|
      RawTable (
        model name: "{unHaskellName $ entityHaskell model}", \
        table name (as in database): "{unDBName $ entityDB model}"
      )
    |]

  show (RawField x) = go where
    model = entityDefByField x
    field = persistFieldDef  x

    go = [qm|
      RawField (
        model name: "{unHaskellName $ entityHaskell model}", \
        table name (as in database): "{unDBName $ entityDB model}", \
        field name: "{unHaskellName $ fieldHaskell field}", \
        field name (as in database): "{unDBName $ fieldDB field}"
      )
    |]

  show (RawValue x) = [qm| RawValue ({show x}) |]

  show (RawFieldValue x y) = go where
    model = entityDefByField x
    field = persistFieldDef  x

    go = [qm|
      RawFieldValue (
        model name: "{unHaskellName $ entityHaskell model}", \
        table name (as in database): "{unDBName $ entityDB model}", \
        field name: "{unHaskellName $ fieldHaskell field}", \
        field name (as in database): "{unDBName $ fieldDB field}", \
        value: {show y}
      )
    |]

  show (RawTableAliasFieldValue x y) = go where
    model = entityDefByTableAliasToken     x
    field = fieldDefByTableAliasFieldToken x

    go = [qm|
      RawTableAliasFieldValue (
        model name: "{unHaskellName $ entityHaskell model}", \
        table name (as in database): "{unDBName $ entityDB model}", \
        table alias: "{aliasByTableAliasToken x}", \
        field name: "{unHaskellName $ fieldHaskell field}", \
        field name (as in database): "{unDBName $ fieldDB field}", \
        value: {show y}
      )
    |]

  show (RawTableAlias x) = go where
    model = entityDefByTableAliasToken x

    go = [qm|
      RawTableAlias (
        model name: "{unHaskellName $ entityHaskell model}", \
        table name (as in database): "{unDBName $ entityDB model}", \
        table alias: "{aliasByTableAliasToken x}"
      )
    |]

  show (RawTableAliasField x) = go where
    model = entityDefByTableAliasToken     x
    field = fieldDefByTableAliasFieldToken x

    go = [qm|
      RawTableAliasField (
        model name: "{unHaskellName $ entityHaskell model}", \
        table name (as in database): "{unDBName $ entityDB model}", \
        table alias: "{aliasByTableAliasToken x}",
        field name: "{unHaskellName $ fieldHaskell field}", \
        field name (as in database): "{unDBName $ fieldDB field}"
      )
    |]

  show (RawAliasAs x y) = [qm|
    RawTableAliasField (alias: "{symbolVal x}", innards: {y})
  |]

  show (RawAlias x) = [qm| RawAlias "{symbolVal x}" |]

  show (RawAliasedName x y) = [qm|
    RawAliasedName (alias: "{symbolVal x}", name: "{y}")
  |]

  show (RawWrap x) = [qm| RawWrap ({x}) |]
  show (RawWrap' (x, y) z) = [qm| RawWrap' ("{x}", "{y}") ({z}) |]


------------------------------------------------------------------------------
-- Builder of an SQL request
------------------------------------------------------------------------------


newtype DefinedTableAlias   = DefinedTableAlias   String deriving (Eq, Show)
newtype RequestedTableAlias = RequestedTableAlias String deriving (Eq, Show)
newtype DefinedWithAlias    = DefinedWithAlias    String deriving (Eq, Show)
newtype RequestedWithAlias  = RequestedWithAlias  String deriving (Eq, Show)

-- | For validation.
type AliasesAccumulator =
   ( V.Vector DefinedTableAlias
   , V.Vector RequestedTableAlias
   , V.Vector DefinedWithAlias
   , V.Vector RequestedWithAlias
   )


-- | Constructor of a raw SQL request.
--
-- Usage example:
--
-- @
-- result <-
--  let
--    inferTypes
--      :: forall model a b x
--       . x ~ (Single a, Single b)
--      => Proxy model
--      -> EntityField model a
--      -> EntityField model b
--      -> ReaderT SqlBackend m [x]
--      -> ReaderT SqlBackend m [x]
--
--    inferTypes Proxy _ _ = id
--
--    contract = Proxy :: Proxy Contract
--    idField  = ContractId
--    vinField = ContractVin
--  in
--    inferTypes contract idField vinField
--      $ uncurry rawEsqueletoSql
--      $ buildRawSql
--      [ raw SELECT
--      ,   rawSeq [RawField idField, RawField vinField]
--      , raw FROM
--      ,   getTableByItsField idField
--      , raw WHERE
--      ,   let field = ContractIsActive
--           in RawWrap [RawField field, raw EQUALS, RawFieldValue field True]
--      , raw AND
--      ,   RawWrap [RawField vinField, raw IS, raw NOT, raw NULL]
--      , raw ORDER_BY, RawField idField, raw DESC
--      , raw LIMIT, RawValue (10 :: Word)
--      ]
-- @
buildRawSql
  :: forall f. Foldable f
  => f (RawSqlPiece f)
  -> (Text, [PersistValue])

buildRawSql = x where
  x = foldl pieceReducer ((mempty, mempty, mempty, mempty), mempty, mempty)
    ? \(aliases, sql, values) -> validate aliases `seq` (sql, V.toList values)

  pieceReducer
    :: (AliasesAccumulator, Text, V.Vector PersistValue)
    -> RawSqlPiece f
    -> (AliasesAccumulator, Text, V.Vector PersistValue)

  pieceReducer (aliases, sql, values) (RawPlain plain) =
    (aliases, [qm| {sql}{plain} |], values)

  pieceReducer (aliases, sql, values) (RawTable p@Proxy) = go where
    go = (aliases, [qm| {sql}{show tableDBName} |], values)
    tableDBName = unDBName $ entityDB $ entityDefByModelProxy p

  pieceReducer (aliases, sql, values) (RawField field) = go where
    go = (aliases, [qm| {sql}{show tableDBName}.{show fieldDBName} |], values)
    tableDBName = unDBName $ entityDB $ entityDefByField field
    fieldDBName = unDBName $ fieldDB $ persistFieldDef field

  pieceReducer (aliases, sql, values) (RawValue value) =
    (aliases, [qm| {sql}? |], values `V.snoc` toPersistValue value)

  pieceReducer (aliases, sql, values) (RawFieldValue _ value) =
    (aliases, [qm| {sql}? |], values `V.snoc` toPersistValue value)

  pieceReducer (aliases, sql, values) (RawTableAliasFieldValue t v) = go where
    go = (newAliases, [qm| {sql}? |], values `V.snoc` toPersistValue v)
    alias = RequestedTableAlias $ aliasByTableAliasToken t

    -- | I'm not sure whether we should add requested alias to the accumulator
    --   since we don't really use it here.
    newAliases = case aliases of (a, b, c, d) -> (a, b `V.snoc` alias, c, d)

  pieceReducer (aliases, sql, values) (RawTableAlias token) = go where
    go = (newAliases, [qm| {sql}{show tableDBName} AS {show alias} |], values)
    newAliases = case aliases of (a, b, c, d) -> (a `V.snoc` alias', b, c, d)
    tableDBName = unDBName $ entityDB entity
    entity = entityDefByTableAliasToken token
    alias = aliasByTableAliasToken token
    alias' = DefinedTableAlias alias

  pieceReducer (aliases, sql, values) (RawTableAliasField token) = go where
    go = (newAliases, [qm| {sql}{show alias}.{show fieldDBName} |], values)
    newAliases = case aliases of (a, b, c, d) -> (a, b `V.snoc` alias', c, d)
    fieldDBName = unDBName $ fieldDB field
    field = fieldDefByTableAliasFieldToken token
    alias = aliasByTableAliasToken token
    alias' = RequestedTableAlias alias

  pieceReducer (aliases, sql, values) (RawAliasAs as@Proxy children) = go where
    go = (newAliases, [qm| {sql}{show alias} AS ({childrenPlain}) |], newValues)
    newValues = values `mappend` childrenValues
    alias' = DefinedWithAlias alias
    alias = symbolVal as

    newAliases =
      case (aliases, childrenAliases) of
           ((a, b, c, d), (b', d')) ->
             ( a
             , b `mappend` b'
             , c `V.snoc` alias'
             , d `mappend` d'
             )

    childrenInitAcc = case aliases of (a, _, c, _) -> (a, mempty, c, mempty)

    (childrenAliases, childrenPlain, childrenValues)
      = foldl pieceReducer (childrenInitAcc, mempty, mempty) children
      & \(aliases'@(_, b, _, d), sql', values') ->
          validate aliases' `seq` ((b, d), sql', values')

  pieceReducer (aliases, sql, values) (RawAlias as@Proxy) = go where
    go = (newAliases, [qm| {sql}{show alias} |], values)
    newAliases = case aliases of (a, b, c, d) -> (a, b, c, d `V.snoc` alias')
    alias' = RequestedWithAlias alias
    alias = symbolVal as

  pieceReducer (aliases, sql, values) (RawAliasedName as@Proxy n) = go where
    go = (newAliases, [qm| {sql}{show alias}.{show n} |], values)
    newAliases = case aliases of (a, b, c, d) -> (a, b, c, d `V.snoc` alias')
    alias' = RequestedWithAlias alias
    alias = symbolVal as

  pieceReducer (aliases, sql, values) (RawWrap children) = go where
    go = (newAliases, [qm| {sql}({childrenPlain}) |], newValues)
    newValues = values `mappend` childrenValues

    (newAliases, childrenPlain, childrenValues)
      = foldl pieceReducer (aliases, mempty, mempty) children
      & \result@(aliases', _, _) -> validate aliases' `seq` result

  pieceReducer (aliases, sql, values) (RawWrap' (l, r) children) = go where
    go = (newAliases, [qm| {sql}{l}{childrenPlain}{r} |], newValues)
    newValues = values `mappend` childrenValues

    (newAliases, childrenPlain, childrenValues)
      = foldl pieceReducer (aliases, mempty, mempty) children
      & \result@(aliases', _, _) -> validate aliases' `seq` result

  -- | TODO implement
  --
  -- TODO check that all contextual aliased names are have their aliases
  --      defined, keep in mind that an alias can be defined after it's being
  --      used.
  --
  -- TODO "RawAliasAs" shouldn't export any aliases to upper level except only
  --      the alias of itself (first @Proxy@ parameter of constructor).
  --
  -- TODO "RawAliasAs" should inherit defined aliases from upper level.
  --      It might be tricky since names from upper level may be defined after
  --      they are being used.
  --
  -- TODO check that all aliases have unique name, do not allow to declare alias
  --      with the same name more than once.
  --      It might be tricky to check such name duplicated inside children of
  --      "RawAliasAs".
  --
  -- This validator wouldn't give you total runtime proof but can cover most of
  -- the typical errors caused by a human.
  --
  -- It should just end with "error" function with some proper error message in
  -- case of failure of validation.
  validate :: AliasesAccumulator -> ()
  validate (_, _, _, _) = ()


------------------------------------------------------------------------------
-- Safe constructors of alias tokens
------------------------------------------------------------------------------


mkTableAliasToken
  :: forall model alias. (PersistEntity model, KnownSymbol alias)
  => TableAliasToken model alias

mkTableAliasToken = TableAliasToken


mkTableAliasFieldToken
  :: forall model alias fieldType
   . (PersistEntity model, KnownSymbol alias, Typeable fieldType)
  => TableAliasToken model alias
  -> EntityField model fieldType
  -> TableAliasFieldToken model alias fieldType

mkTableAliasFieldToken TableAliasToken = TableAliasFieldToken


------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------


-- | @fieldType@ doesn't matter here.
entityDefByField
  :: forall model fieldType. PersistEntity model
  => EntityField model fieldType
  -> EntityDef

entityDefByField _ = entityDef (pure undefined :: Identity model)


entityDefByModelProxy
  :: forall model. PersistEntity model
  => Proxy model
  -> EntityDef

entityDefByModelProxy Proxy = entityDef (pure undefined :: Identity model)


class EntityDefByTableAliasToken t where
  entityDefByTableAliasToken :: t -> EntityDef

instance ( PersistEntity model
         , KnownSymbol alias
         ) => EntityDefByTableAliasToken (TableAliasToken model alias) where

  entityDefByTableAliasToken TableAliasToken =
    entityDef (pure undefined :: Identity model)

instance ( PersistEntity model
         , KnownSymbol alias
         , Typeable fieldType
         ) => EntityDefByTableAliasToken
         ( TableAliasFieldToken model alias fieldType
         ) where

  entityDefByTableAliasToken (TableAliasFieldToken _) =
    entityDef (pure undefined :: Identity model)


class AliasByTableAliasToken t where
  aliasByTableAliasToken :: t -> String

instance ( PersistEntity model
         , KnownSymbol alias
         ) => AliasByTableAliasToken (TableAliasToken model alias) where

  aliasByTableAliasToken TableAliasToken = symbolVal (Proxy :: Proxy alias)

instance ( PersistEntity model
         , KnownSymbol alias
         , Typeable fieldType
         ) => AliasByTableAliasToken
         ( TableAliasFieldToken model alias fieldType
         ) where

  aliasByTableAliasToken (TableAliasFieldToken _) =
    symbolVal (Proxy :: Proxy alias)


fieldDefByTableAliasFieldToken
  :: forall model alias fieldType
   . (PersistEntity model, KnownSymbol alias, Typeable fieldType)
  => TableAliasFieldToken model alias fieldType
  -> FieldDef

fieldDefByTableAliasFieldToken (TableAliasFieldToken x) = persistFieldDef x


raw :: Foldable f => RawBasic -> RawSqlPiece f
raw = RawPlain . (\x -> " " `mappend` x `mappend` " ") . \case
  SELECT            -> "SELECT"
  FROM              -> "FROM"
  WHERE             -> "WHERE"
  ORDER_BY          -> "ORDER BY"
  ASC               -> "ASC"
  DESC              -> "DESC"
  LIMIT             -> "LIMIT"
  OFFSET            -> "OFFSET"
  WITH              -> "WITH"
  AND               -> "AND"
  OR                -> "OR"
  STAR              -> "*"
  COMMA             -> ","
  SEMICOLON         -> ";"
  EQUALS            -> "="
  GREATER           -> ">"
  GREATER_OR_EQUALS -> ">="
  LESS              -> "<"
  LESS_OR_EQUALS    -> "<="
  NOT               -> "NOT"
  NULL              -> "NULL"
  AS                -> "AS"
  IS                -> "IS"
  IN                -> "IN"
  NOW               -> "NOW()"

data RawBasic
   = SELECT | FROM | WHERE | ORDER_BY | ASC | DESC | LIMIT | OFFSET | WITH
   | AND | OR | STAR | COMMA | SEMICOLON | EQUALS | GREATER | LESS
   | GREATER_OR_EQUALS | LESS_OR_EQUALS | NOT | NULL | AS | IS | IN | NOW
     deriving (Eq, Show)


rawSelectAlias
  ::
   ( Cons' f result
   , Foldable f
   , Applicative f
   , Show (f result)
   , Eq (f result)
   , KnownSymbol a
   , result ~ RawSqlPiece f
   )
  => Proxy a
  -> result

rawSelectAlias p@Proxy
  = RawWrap
  $ raw SELECT <| raw STAR <| raw FROM <| pure (RawAlias p)


-- | Wraps list into parenthesis and separates its values with comma.
rawIntersected
  ::
   ( Cons' f piece
   , Foldable f
   , Monoid (f piece)
   , Show (f piece)
   , Eq (f piece)
   , piece ~ RawSqlPiece f
   )
  => (Text, Text)
  -> piece
  -> f piece
  -> piece

rawIntersected openCloseSymbols separator =
  uncons ? \case
    Nothing -> RawPlain mempty
    Just (x, xs) ->
      RawWrap' openCloseSymbols $
        x <| foldl (\acc y -> separator <| y <| acc) mempty xs


-- | Just separates values with comma automatically.
rawSeq
  ::
   ( Cons' f piece
   , Foldable f
   , Monoid (f piece)
   , Show (f piece)
   , Eq (f piece)
   , piece ~ RawSqlPiece f
   )
  => f piece
  -> piece

rawSeq = rawIntersected (mempty, mempty) $ raw COMMA


getTableByItsField
  :: forall model fieldType f
   . (PersistEntity model, Typeable fieldType, Foldable f)
  => EntityField model fieldType
  -> RawSqlPiece f

getTableByItsField _ = RawTable (Proxy :: Proxy model)
