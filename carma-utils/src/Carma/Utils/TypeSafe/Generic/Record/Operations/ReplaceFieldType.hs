{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Carma.Utils.TypeSafe.Generic.Record.Operations.ReplaceFieldType
     ( type ReplaceFieldTypeFromToByFieldName
     , type ReplaceFieldTypeFromTo
     ) where

import           GHC.Generics
import           GHC.TypeLits


-- | Replaces type of field by its name.
--
-- You have to provide correct field name, original type of that field and new
-- type to replace field's type to.
--
-- As a result you get a pair of @Nat@ with number of all replacements (so you
-- could match correct expected amount of replacements) and new @Generic@ type
-- representation.
--
-- When total replacements number is biggen than @1@? You could for example have
-- two constructors with same field in each one and with same type of that
-- field.
--
-- If a field name and/or its original type don't match then a field will be
-- just skipped and left as is.
type family ReplaceFieldTypeFromToByFieldName (k1 :: * -> *)
                                              (k2 :: Symbol)
                                              (k3 :: *)
                                              (k4 :: *)
                                                  :: (Nat, * -> *) where

  ReplaceFieldTypeFromToByFieldName (D1 a b) fieldName fromType toType =
    ReplaceFieldTypeFromToByFieldNameInternal
      (D1 a b) fieldName fromType toType 0


-- | See "ReplaceFieldTypeFromToByFieldName".
type family ReplaceFieldTypeFromToByFieldNameInternal (k1 :: * -> *)
                                                      (k2 :: Symbol)
                                                      (k3 :: *)
                                                      (k4 :: *)
                                                      (k5 :: Nat)
                                                          :: (Nat, * -> *) where

  ReplaceFieldTypeFromToByFieldNameInternal
    (D1 a (C1 ('MetaCons b c 'True) x)) fieldName fromType toType acc =
      ReplaceFieldTypeFromToInternalWrapWith
        (D1 a)
        (
          ReplaceFieldTypeFromToInternalWrapWith
            (C1 ('MetaCons b c 'True))
            (
              ReplaceFieldTypeFromToByFieldNameInternal
                x fieldName fromType toType acc
            )
        )

  ReplaceFieldTypeFromToByFieldNameInternal
    (D1 a (C1 ('MetaCons b c 'True) x :+: xs)) fieldName fromType toType acc =
      ReplaceFieldTypeFromToInternalWrapWith
        (D1 a)
        (
          ReplaceFieldTypeFromToInternalSumWith
            (:+:)
            (
              ReplaceFieldTypeFromToInternalWrapWith
                (C1 ('MetaCons b c 'True))
                (
                  ReplaceFieldTypeFromToByFieldNameInternal
                    x fieldName fromType toType acc
                )
            )
            (
              ReplaceFieldTypeFromToByFieldNameInternal
                xs fieldName fromType toType 0
            )
        )

  -- Skipping constructors with no record fields
  ReplaceFieldTypeFromToByFieldNameInternal
    (D1 a (C1 ('MetaCons b c 'False) x)) _ _ _ acc =
      '(acc, (D1 a (C1 ('MetaCons b c 'False) x)))

  -- Skipping constructors with no record fields
  ReplaceFieldTypeFromToByFieldNameInternal
    (D1 a (C1 ('MetaCons b c 'False) x :+: xs)) fieldName fromType toType acc =
      ReplaceFieldTypeFromToInternalWrapWith
        (D1 a)
        (
          ReplaceFieldTypeFromToInternalSumWith
            (:+:)
            '(acc, C1 ('MetaCons b c 'False) x)
            (
              ReplaceFieldTypeFromToByFieldNameInternal
                xs fieldName fromType toType 0
            )
        )

  ReplaceFieldTypeFromToByFieldNameInternal
    (D1 a (xs :+: ys)) fieldName fromType toType acc =
      ReplaceFieldTypeFromToInternalWrapWith
        (D1 a)
        (
          ReplaceFieldTypeFromToInternalSumWith
            (:+:)
            (
              ReplaceFieldTypeFromToByFieldNameInternal
                xs fieldName fromType toType acc
            )
            (
              ReplaceFieldTypeFromToByFieldNameInternal
                ys fieldName fromType toType 0
            )
        )


  -- Match!
  ReplaceFieldTypeFromToByFieldNameInternal
    (S1 ('MetaSel ('Just fieldName) a b c) (Rec0 fromType))
    fieldName fromType toType acc =
      '(acc + 1, S1 ('MetaSel ('Just fieldName) a b c) (Rec0 toType))

  -- Match!
  ReplaceFieldTypeFromToByFieldNameInternal
    (S1 ('MetaSel ('Just fieldName) a b c) (Rec0 fromType) :*: xs)
    fieldName fromType toType acc =
      ReplaceFieldTypeFromToInternalSumWith
        (:*:)
        '(1, S1 ('MetaSel ('Just fieldName) a b c) (Rec0 toType))
        (
          ReplaceFieldTypeFromToByFieldNameInternal
            xs fieldName fromType toType acc
        )

  ReplaceFieldTypeFromToByFieldNameInternal
    (S1 ('MetaSel a b c d) e :*: xs) fieldName fromType toType acc =
      ReplaceFieldTypeFromToInternalSumWith
        (:*:)
        '(0, S1 ('MetaSel a b c d) e)
        (
          ReplaceFieldTypeFromToByFieldNameInternal
            xs fieldName fromType toType acc
        )

  ReplaceFieldTypeFromToByFieldNameInternal
    (xs :*: ys) fieldName fromType toType acc =
      ReplaceFieldTypeFromToInternalSumWith
        (:*:)
        (
          ReplaceFieldTypeFromToByFieldNameInternal
            xs fieldName fromType toType acc
        )
        (
          ReplaceFieldTypeFromToByFieldNameInternal
            ys fieldName fromType toType 0
        )


-- | A version of "ReplaceFieldTypeFromToByFieldName" which not constrained by
--   specific field name, it replaces every matched type of every field.
type family ReplaceFieldTypeFromTo (k1 :: * -> *)
                                   (k2 :: *)
                                   (k3 :: *)
                                       :: (Nat, * -> *)
                                       where

  ReplaceFieldTypeFromTo (D1 a b) fromType toType =
    ReplaceFieldTypeFromToInternal (D1 a b) fromType toType 0


-- | See "ReplaceFieldTypeFromTo".
type family ReplaceFieldTypeFromToInternal (k1 :: * -> *)
                                           (k2 :: *)
                                           (k3 :: *)
                                           (k4 :: Nat)
                                               :: (Nat, * -> *)
                                               where

  ReplaceFieldTypeFromToInternal
    (D1 a (C1 ('MetaCons b c 'True) x)) fromType toType acc =
      ReplaceFieldTypeFromToInternalWrapWith
        (D1 a)
        (
          ReplaceFieldTypeFromToInternalWrapWith
            (C1 ('MetaCons b c 'True))
            (ReplaceFieldTypeFromToInternal x fromType toType acc)
        )

  ReplaceFieldTypeFromToInternal
    (D1 a (C1 ('MetaCons b c 'True) x :+: xs)) fromType toType acc =
      ReplaceFieldTypeFromToInternalWrapWith
        (D1 a)
        (
          ReplaceFieldTypeFromToInternalSumWith
            (:+:)
            (
              ReplaceFieldTypeFromToInternalWrapWith
                (C1 ('MetaCons b c 'True))
                (ReplaceFieldTypeFromToInternal x fromType toType acc)
            )
            (ReplaceFieldTypeFromToInternal xs fromType toType 0)
        )

  -- Skipping constructors with no record fields
  ReplaceFieldTypeFromToInternal (D1 a (C1 ('MetaCons b c 'False) x)) _ _ acc =
    '(acc, (D1 a (C1 ('MetaCons b c 'False) x)))

  -- Skipping constructors with no record fields
  ReplaceFieldTypeFromToInternal
    (D1 a (C1 ('MetaCons b c 'False) x :+: xs)) fromType toType acc =
      ReplaceFieldTypeFromToInternalWrapWith
        (D1 a)
        (
          ReplaceFieldTypeFromToInternalSumWith
            (:+:)
            '(acc, C1 ('MetaCons b c 'False) x)
            (ReplaceFieldTypeFromToInternal xs fromType toType 0)
        )

  ReplaceFieldTypeFromToInternal
    (D1 a (xs :+: ys)) fromType toType acc =
      ReplaceFieldTypeFromToInternalWrapWith
        (D1 a)
        (
          ReplaceFieldTypeFromToInternalSumWith
            (:+:)
            (ReplaceFieldTypeFromToInternal xs fromType toType acc)
            (ReplaceFieldTypeFromToInternal ys fromType toType 0)
        )


  -- Match!
  ReplaceFieldTypeFromToInternal
    (S1 ('MetaSel ('Just a) b c d) (Rec0 fromType)) fromType toType acc =
      '(acc + 1, S1 ('MetaSel ('Just a) b c d) (Rec0 toType))

  -- Match!
  ReplaceFieldTypeFromToInternal
    (S1 ('MetaSel ('Just a) b c d) (Rec0 fromType) :*: xs) fromType toType acc =
      ReplaceFieldTypeFromToInternalSumWith
        (:*:)
        '(1, S1 ('MetaSel ('Just a) b c d) (Rec0 toType))
        (ReplaceFieldTypeFromToInternal xs fromType toType acc)

  ReplaceFieldTypeFromToInternal
    (S1 ('MetaSel a b c d) e :*: xs) fromType toType acc =
      ReplaceFieldTypeFromToInternalSumWith
        (:*:)
        '(0, S1 ('MetaSel a b c d) e)
        (ReplaceFieldTypeFromToInternal xs fromType toType acc)

  ReplaceFieldTypeFromToInternal (xs :*: ys) fromType toType acc =
    ReplaceFieldTypeFromToInternalSumWith
      (:*:)
      (ReplaceFieldTypeFromToInternal xs fromType toType acc)
      (ReplaceFieldTypeFromToInternal ys fromType toType 0)


-- | A helper.
type family ReplaceFieldTypeFromToInternalWrapWith
              (k1 :: (* -> *) -> (* -> *))
              (k2 :: (Nat, * -> *))
                  :: (Nat, * -> *)
                  where
  ReplaceFieldTypeFromToInternalWrapWith wrapFn '(n, x) =
    '(n, wrapFn x)


-- | A helper.
type family ReplaceFieldTypeFromToInternalSumWith
              (k1 :: (* -> *) -> (* -> *) -> (* -> *))
              (k2 :: (Nat, * -> *))
              (k3 :: (Nat, * -> *))
                  :: (Nat, * -> *)
                  where
  ReplaceFieldTypeFromToInternalSumWith sumFn '(nA, a) '(nB, b) =
    '(nA + nB, sumFn a b)
