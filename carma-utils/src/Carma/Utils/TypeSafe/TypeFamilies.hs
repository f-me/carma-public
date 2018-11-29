{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Carma.Utils.TypeSafe.TypeFamilies
     ( type MaybeAlternative

     , type MaybePair
     , type MaybeTriplet

     , type TypeIsMaybe
     , type MaybeTypeIsMaybe

     , type LiftFstMaybe
     , type LiftSndMaybe

     , type Not
     , type MaybeNot

     , type MaybeList
     , type Uncons
     , type MaybeCons
     , type MaybeListCons
     , type MaybeMaybeCons
     , type Concat
     , type MaybeMaybeConcat
     ) where


-- | Just like @(<|>)@ operator of "Alternative" instance for "Maybe" only on
-- type-level.
type family MaybeAlternative (k1 :: Maybe k)
                             (k2 :: Maybe k)
                                 :: Maybe k where
  MaybeAlternative ('Just x) _        = 'Just x
  MaybeAlternative 'Nothing ('Just x) = 'Just x
  MaybeAlternative 'Nothing 'Nothing  = 'Nothing


-- | Constructs a type-level pair tuple of two "Maybe" values.
--
-- Runtime-level analogy: like "Applicative" "Functor", @(,) <$> a <*> b@.
type family MaybePair (k1 :: Maybe kk1)
                      (k2 :: Maybe kk2)
                          :: Maybe (kk1, kk2) where
  MaybePair ('Just a) ('Just b) = 'Just '(a, b)
  MaybePair _ _ = 'Nothing

-- | Constructs a type-level triplet tuple of three "Maybe" values.
--
-- Runtime-level analogy: like "Applicative" "Functor",
-- @(,,) <$> a <*> b <*> c@.
type family MaybeTriplet (k1 :: Maybe kk1)
                         (k2 :: Maybe kk2)
                         (k3 :: Maybe kk3)
                             :: Maybe (kk1, kk2, kk3) where
  MaybeTriplet ('Just a) ('Just b) ('Just c) = 'Just '(a, b, c)
  MaybeTriplet _ _ _ = 'Nothing


-- | Checks if provided type is wrapped to "Maybe"
type family TypeIsMaybe (k1 :: *) :: Bool where
  TypeIsMaybe (Maybe _) = 'True
  TypeIsMaybe _         = 'False

-- | Checks if provided type is wrapped to "Maybe" but takes a type inside
-- "Maybe" itself.
--
-- Runtime-level analogy: like "Applicative" "Functor" version of "TypeIsMaybe".
type family MaybeTypeIsMaybe (k1 :: Maybe *) :: Maybe Bool where
  MaybeTypeIsMaybe 'Nothing  = 'Nothing
  MaybeTypeIsMaybe ('Just x) = 'Just (TypeIsMaybe x)


-- | When first element of a tuple is "Maybe", it unwraps it and returns a
-- "Maybe" of tuple with unwrapped values.
--
-- If first tuple element is "Nothing" then result will be also "Nothing".
--
-- Runtime-level analogy: @f (a, b) = a <&> (, b)@.
type family LiftFstMaybe (k1 :: (Maybe a, b)) :: Maybe (a, b) where
  LiftFstMaybe '( 'Just a, b ) = 'Just '(a, b)
  LiftFstMaybe _ = 'Nothing

-- | When second element of a tuple is "Maybe", it unwraps it and returns a
-- "Maybe" of tuple with unwrapped values.
--
-- If second tuple element is "Nothing" then result will be also "Nothing".
--
-- Runtime-level analogy: @f (a, b) = (a,) <$> b@.
type family LiftSndMaybe (k1 :: (a, Maybe b)) :: Maybe (a, b) where
  LiftSndMaybe '( a, 'Just b ) = 'Just '(a, b)
  LiftSndMaybe _ = 'Nothing


-- | Just inverts type-level "Bool"
type family Not (k1 :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

-- | Alternative version of "Not" when value is wrapped to "Maybe".
--
-- Runtime-level analogy: @fmap not@.
type family MaybeNot (k1 :: Maybe Bool) :: Maybe Bool where
  MaybeNot 'Nothing  = 'Nothing
  MaybeNot ('Just x) = 'Just (Not x)


-- | Constructs a type-level list of a single provided element but provided
-- element (type) is wrapped inside "Maybe".
--
-- Runtime-level analogy: kinda like "Functor", @fmap (:[])@.
type family MaybeList (k1 :: Maybe a) :: Maybe [a] where
  MaybeList ('Just x) = 'Just '[x]
  MaybeList _ = 'Nothing

-- | Type-level version of "Data.List.uncons".
type family Uncons (k1 :: [a]) :: Maybe (a, [a]) where
  Uncons '[] = 'Nothing
  Uncons (x ': xs) = 'Just '(x, xs)

-- | A version of @':@ when a value we are \"cons"-ing is wrapped to "Maybe".
--
-- When a value is "Nothing" result is also "Nothing".
--
-- Runtime-level analogy: @value <&> (: list)@.
type family MaybeCons (k1 :: Maybe a) (k2 :: [a]) :: Maybe [a] where
  MaybeCons ('Just x) xs = 'Just (x ': xs)
  MaybeCons _ _ = 'Nothing

-- | A version of @':@ when a list to which we are \"cons"-ing is wrapped to
-- "Maybe".
--
-- When a list is "Nothing" result is also "Nothing".
--
-- Runtime-level analogy: @(value :) <$> list@.
type family MaybeListCons (k1 :: a) (k2 :: Maybe [a]) :: Maybe [a] where
  MaybeListCons x ('Just xs) = 'Just (x ': xs)
  MaybeListCons _ _ = 'Nothing

-- | A version of @':@ when both a value and a list to which we are \"cons"-ing
-- are wrapped to "Maybe".
--
-- When a value and/or a list is "Nothing" result is also "Nothing".
--
-- Runtime-level analogy: @(:) <$> value <*> list@.
type family MaybeMaybeCons (k1 :: Maybe a) (k2 :: Maybe [a]) :: Maybe [a] where
  MaybeMaybeCons ('Just x) ('Just xs) = 'Just (x ': xs)
  MaybeMaybeCons _ _ = 'Nothing

-- | Type-level version of @(++)@ operator.
type family Concat (k1 :: [a]) (k2 :: [a]) :: [a] where
  Concat '[] xs = xs
  Concat (x ': xs) ys = x ': Concat xs ys

-- | Alternative version of "Concat" when provided lists are wrapped to "Maybe".
--
-- When either list is "Nothing" result is also "Nothing".
--
-- Runtime-level analogy: @(++) <$> listA <*> listB@.
type family MaybeMaybeConcat (k1 :: Maybe [a])
                             (k2 :: Maybe [a])
                                 :: Maybe [a] where
  MaybeMaybeConcat 'Nothing _ = 'Nothing
  MaybeMaybeConcat _ 'Nothing = 'Nothing
  MaybeMaybeConcat ('Just '[]) ('Just ys) = 'Just ys
  MaybeMaybeConcat ('Just (x ': xs)) ('Just ys) = 'Just (x ': Concat xs ys)
