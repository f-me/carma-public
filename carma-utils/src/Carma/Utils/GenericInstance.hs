{-# LANGUAGE ExplicitNamespaces, DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, DeriveGeneric #-}

-- | Some helpers to write "GHC.Generics.Generic" instance manually.
--
-- This could be useful when you have a GADT with some type equality constraints
-- for some constructors which makes "GHC.Generics.Generic" instance being
-- underivable for that GADT so you have to implement "GHC.Generics.Generic"
-- instance manually.
--
-- See also the original source of this helpers:
-- https://gist.github.com/unclechu/05f3f0f3aa3c49c51467ec476e91ffcf
--
-- TODO Implement constructor with fields but without records.
module Carma.Utils.GenericInstance
     ( type MetaDataOf
     , type ProtoDatatype
     , type EmptyConstructor
     , type RecordConstructor
     ) where

import GHC.Generics
import GHC.TypeLits


-- | To inherit "GHC.Generics.MetaData" from another datatype which has
--   automatically derived "GHC.Generics.Generic" instance and replace name of
--   the type.
--
-- To automatically obtain current module name and current package name of the
-- type.
--
-- WARNING! It's urgently recommended to use some type from the same module,
-- otherwise module name or package name will be incorrect for type you're
-- implementing "GHC.Generics.Generic" instance for.
--
-- If you don't have such type in the same module, then just define dummy one:
--
-- @
-- data DummyGeneric deriving Generic
-- @
type family MetaDataOf (typeRep :: * -> *) (typeName :: Symbol) :: Meta where
  MetaDataOf (D1 ('MetaData _ module' package isNewType) _) typeName =
    'MetaData typeName module' package isNewType

-- | Using some "GHC.Generics.Generic" type representation as a prototype for
--   "GHC.Generics.MetaData" for type you're implementing "GHC.Generics.Generic"
--   instance for.
--
-- Usage example:
--
-- @
-- -- ...
-- data Scenario = ScenarioA | ScenarioB deriving Generic
--
-- data Foo (s :: Scenario) where
--   Constructor1 :: { foo :: Int,  bar :: String } -> Foo s
--   Constructor2 :: { baz :: Int,  bzz :: Bool   } -> Foo 'ScenarioA
--   Constructor3 :: { zzz :: Bool, xxx :: String } -> Foo 'ScenarioB
-- -- ...
-- instance Generic (Foo 'ScenarioA) where
--   type Rep (Foo 'ScenarioA) =
--     ProtoDatatype (Rep Scenario) "Foo"
--       '[ RecordConstructor "Constructor1"
--            '[ '("foo", Int)
--             , '("bar", String)
--             ]
--        , RecordConstructor "Constructor2"
--            '[ '("baz", Int)
--             , '("bzz", Bool)
--             ]
--        ]
-- -- ...
-- instance Generic (Foo 'ScenarioB) where
--   type Rep (Foo 'ScenarioB) =
--     ProtoDatatype (Rep Scenario) "Foo"
--       '[ RecordConstructor "Constructor1"
--            '[ '("foo", Int)
--             , '("bar", String)
--             ]
--        , RecordConstructor "Constructor3"
--            '[ '("zzz", Bool)
--             , '("xxx", String)
--             ]
--        ]
-- -- ...
-- @
type ProtoDatatype (proto :: * -> *) (name :: Symbol) (constructors :: [* -> *])
   = D1 (MetaDataOf proto name) (ProtoDatatypeC constructors)

type family ProtoDatatypeC (constructors :: [* -> *]) :: * -> * where
  ProtoDatatypeC '[x] = x
  ProtoDatatypeC (x ': xs) = x :+: ProtoDatatypeC xs

type EmptyConstructor (name :: Symbol) = C1 ('MetaCons name 'PrefixI 'False) U1

type RecordConstructor (name :: Symbol) (fields :: [(Symbol, *)])
   = C1 ('MetaCons name 'PrefixI 'True) (RecordConstructorF fields)

type family RecordConstructorF (fields :: [(Symbol, *)]) :: * -> * where
  RecordConstructorF '[ '(name, t) ] =
    S1 ( 'MetaSel ('Just name)
                  'NoSourceUnpackedness
                  'NoSourceStrictness
                  'DecidedLazy
       ) (Rec0 t)

  RecordConstructorF (x ': xs) =
    RecordConstructorF '[x] :*: RecordConstructorF xs
