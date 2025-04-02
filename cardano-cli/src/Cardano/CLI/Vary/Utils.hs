{-

Copyright © 2024 Marten Wijnja (Qqwy)

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the “Software”), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

-}
{-# HLINT ignore "Use camelCase" #-}
-- <- We want a fun long type name with underscores for easier to read errors ;-)
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Vary.Utils
  ( -- |
    -- This module contains functions and typeclasses/type families (type-level functions)
    -- that are not useful in every day usage,
    -- but are sometimes _very_ useful in:
    --
    -- - highly generic code
    -- - When you want to implement typeclasses for 'Vary'.
    -- - When you want to have access to the internals of Vary to debug something

    -- * Useful in generic code and when implementing typeclasses
    (:|)
  , Subset (..)
  , Mappable
  , Length
  , Index
  , IndexOf
  , pop

    -- * Informational (for Debugging)
  , size
  , activeIndex

    -- * Helper functions
  , natValue
  )
where

import Cardano.CLI.Vary.Core (Vary (..), pop)

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits
  ( ErrorMessage (ShowType, Text, (:$$:), (:<>:))
  , KnownNat
  , Nat
  , TypeError
  , natVal
  , type (+)
  , type (-)
  )

-- | Constrain `es` to be any type list containing `e`.
--
-- Useful to talk about variants generically without having to specify the exact type list right away.
--
-- For instance, the type of `Vary.from` is
--
-- > Vary.from :: (a :| l) => a -> Vary l
--
-- because we can use it to construct /any/ Vary as long as there is an @a@ somewhere in its list of types.
type (:|) e es = Member e es

-- | Returns the number of elements contained in this variant.
--
-- Does not actually use the runtime representation of the variant in any way.
size :: forall xs. KnownNat (Length xs) => Vary xs -> Word
size _ = natValue @(Length xs)

-- | Returns the currently active 'tag index' of the variant.
--
-- Not useful in normal code, but maybe nice in certaing debugging scenarios.
--
-- Note that this index changes whenever a variant is `Vary.morph`ed.
activeIndex :: Vary a -> Word
activeIndex (Vary idx _) = idx

-- | Provide evidence that @xs@ is a subset of @es@.
--
-- This is used to make 'Vary.morph' and 'Vary.VEither.morph' work.
class KnownPrefix es => Subset (xs :: [Type]) (es :: [Type]) where
  subsetFullyKnown :: Bool
  subsetFullyKnown =
    -- Don't show "minimal complete definition" in haddock.
    error "subsetFullyKnown"

  morph' :: Vary xs -> Vary ys
  morph' =
    -- Don't show "minimal complete definition" in haddock.
    -- Also, default for the empty instance :-)
    error "morph' was unexpectedly called"

-- If the subset is not fully known, make sure the subset and the base stack
-- have the same unknown suffix.
instance
  {-# INCOHERENT #-}
  ( KnownPrefix es
  , xs `IsUnknownSuffixOf` es
  )
  => Subset xs es
  where
  subsetFullyKnown = False

-- If the subset is fully known, we're done.
instance KnownPrefix es => Subset '[] es where
  subsetFullyKnown = True

instance (e :| es, Subset xs es) => Subset (e : xs) es where
  subsetFullyKnown = subsetFullyKnown @xs @es

  morph' (Vary 0 a) = Vary (natValue @(IndexOf e es)) a
  morph' (Vary n a) = morph' @xs @es (Vary (n - 1) a)

----

-- | Calculate length of a statically known prefix of @es@.
--
-- Used as part of `Subset`.
class KnownPrefix (es :: [Type]) where
  prefixLength :: Int

instance KnownPrefix es => KnownPrefix (e : es) where
  prefixLength = 1 + prefixLength @es

instance {-# INCOHERENT #-} KnownPrefix es where
  prefixLength = 0

----

-- | Require that @xs@ is the unknown suffix of @es@.
--
-- Used as part of `Subset`.
class (xs :: [k]) `IsUnknownSuffixOf` (es :: [k])

instance {-# INCOHERENT #-} xs ~ es => xs `IsUnknownSuffixOf` es

instance xs `IsUnknownSuffixOf` es => xs `IsUnknownSuffixOf` (e : es)

-- | Type-level function to compute the length of a type-level list
type family Length (xs :: [k]) :: Nat where
  Length xs = Length' 0 xs

type family Length' n (xs :: [k]) :: Nat where
  Length' n '[] = n
  Length' n (x ': xs) = Length' (n + 1) xs

-- | A slight generalization of 'GHC.TypeLits.natVal' to return arbitrary 'Num'.
--
-- (List indexes are never negative, after all.)
natValue :: forall (n :: Nat) a. (KnownNat n, Num a) => a
{-# INLINEABLE natValue #-}
natValue = fromIntegral (natVal (Proxy :: Proxy n))

-- | Constraint to link the input and output lists together, without specifying any particular element order.
--
-- This allows us to defer type signatures until the final place the variant is used.
type Mappable a b xs ys = (a :| xs, b :| ys, ys ~ Mapped a b xs)

-- | Compute a HList where the type a was changed into b.
type family Mapped (a :: Type) (b :: Type) (as :: [Type]) = (bs :: [Type]) where
  Mapped a b (a ': as) = (b ': as)
  Mapped a b (x ': as) = x ': Mapped a b as
  Mapped a b l =
    TypeError
      ( 'Text "Cannot map from " ':<>: 'ShowType a ':<>: 'Text " into " ':<>: 'ShowType b
          :$$: 'Text "as it cannot be found in the list " ':<>: 'ShowType l
      )

-- | Look up the index a particular type has in a type-level-list.
--
-- This index is what is used to determine the tag value stored in a 'Vary'.
type IndexOf (x :: k) (xs :: [k]) = IndexOf' (MaybeIndexOf x xs) x xs

-- | Get the first index of a type
type family IndexOf' (i :: Nat) (a :: k) (l :: [k]) :: Nat where
  IndexOf' 0 x l =
    TypeError
      ( 'ShowType x
          ':<>: 'Text " not found in list:"
          ':$$: 'Text " "
            ':<>: 'ShowType l
      )
  IndexOf' i _ _ = i - 1

-- | Get the first index (starting from 1) of a type or 0 if none
type family MaybeIndexOf (a :: k) (l :: [k]) where
  MaybeIndexOf x xs = MaybeIndexOf' 0 x xs

-- | Helper for MaybeIndexOf
type family MaybeIndexOf' (n :: Nat) (a :: k) (l :: [k]) where
  MaybeIndexOf' n x '[] = 0
  MaybeIndexOf' n x (x ': xs) = n + 1
  MaybeIndexOf' n x (y ': xs) = MaybeIndexOf' (n + 1) x xs

-- | Given a type-level index, look up the type at that index.
--
-- If you ever see the @Type_List_Too_Vague...@ in a type error,
-- it means that you need to make the (prefix) of the list of types more concrete
-- by adding some type annotations somewhere.
type Index (n :: Nat) (l :: [k]) =
  Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location n l l

-- | We use this ridiculous name
-- to make it clear to the user when they see it in a type error
-- how to resolve that type error.
type family
  Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location
    (n :: Nat)
    (l :: [k])
    (l2 :: [k])
    :: k
  where
  Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location
    0
    (x ': _)
    _ =
    x
  Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location
    n
    (_ ': xs)
    l2 =
    Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location
      (n - 1)
      xs
      l2
  Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location
    n
    '[]
    l2 =
    TypeError
      ( 'Text "Index "
          ':<>: 'ShowType n
          ':<>: 'Text " out of bounds for list:"
          ':$$: 'Text " "
            ':<>: 'ShowType l2
      )

-- | Constraint: x member of xs
type family Member x xs :: Constraint where
  Member x xs = MemberAtIndex (IndexOf x xs) x xs

type MemberAtIndex i x xs =
  ( x ~ Index i xs
  , KnownNat i
  )

-- | Remove (the first) `a` in `l`
type family Remove (a :: k) (l :: [k]) :: [k] where
  Remove a '[] = '[]
  Remove a (a ': as) = as
  Remove a (b ': as) = b ': Remove a as
