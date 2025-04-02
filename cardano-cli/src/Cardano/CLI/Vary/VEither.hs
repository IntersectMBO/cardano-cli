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
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.CLI.Vary.VEither (
  -- * General Usage
  -- $setup

  -- * Core type definition
  VEither(VLeft, VRight), 
  -- * Conversion
  toVary, 
  fromVary,
  fromLeft,
  fromRight,
  toEither,
  fromEither,
  veither,
  intoOnly,

  -- * case analysis ("pattern matching"):

  -- |
  --
  -- Besides the 'VLeft' and 'VRight' patterns,
  -- 'VEither' supports a bunch of handy combinator functions,
  -- similar to "Vary".'Vary.on' and co.
  onLeft,
  onRight,
  handle,

  -- * Transforming
  mapLeftOn,
  mapLeft,
  mapRight,
  morph,
  morphed,
) where

import Control.Category ((>>>))
import Control.DeepSeq (NFData (..))
import qualified Data.Either
import Data.Kind (Type)
import Cardano.CLI.Vary.Core (Vary(..))
import Cardano.CLI.Vary.Utils (Subset, Mappable)
import Cardano.CLI.Vary ((:|))
import Cardano.CLI.Vary qualified as Vary
import GHC.Generics

# ifdef FLAG_AESON
import qualified Data.Aeson as Aeson
# endif

# ifdef FLAG_HASHABLE
import Data.Hashable
# endif

# ifdef FLAG_QUICKCHECK
import Test.QuickCheck
# endif

# ifdef FLAG_CEREAL
import qualified Data.Serialize as Cereal
# endif

# ifdef FLAG_BINARY
import qualified Data.Binary as Binary
# endif

-- $setup
--
-- This module is intended to be used qualified:
--
-- >>> import Cardano.CLI.Vary.VEither (VEither(VLeft, VRight))
-- >>> import qualified Vary.VEither as VEither
-- 
-- And for many functions, it is useful or outright necessary to enable the following extensions:
--
-- >>> :set -XDataKinds
--
-- Finally, some example snippets in this module make use of 'Data.Function.&', the left-to-right function application operator.
--
-- >>> import Data.Function ((&))


newtype VEither (errs :: [Type]) a = VEither (Vary (a : errs))

-- | Turns the 'VEither' into a normal Vary, no longer considering the @a@ a \'preferred\' value.
--
-- In many cases, you probably want to mattern match on "VEither".'VLeft' instead!
toVary :: VEither errs a -> Vary (a : errs)
{-# INLINE toVary #-}
toVary (VEither vary) = vary

-- | Turns a 'Vary' into a 'VEither'. Now the @a@ is considered the \'preferred\' value.
--
-- In many cases, you probably want to use "VEither".'VLeft' instead!
fromVary :: Vary (a : errs) -> VEither errs a
{-# INLINE fromVary #-}
fromVary vary = VEither vary

-- | Turns a 'VEither' into a normal 'Either'.
toEither :: VEither errs a -> Either (Vary errs) a
{-# INLINE toEither #-}
toEither = toVary >>> Vary.pop

-- | Turns a normal 'Either' into a 'VEither'.
fromEither :: Either (Vary errs) a -> VEither errs a
{-# INLINE fromEither #-}
fromEither = Data.Either.either Vary.morph Vary.from >>> fromVary

-- | Shorthand to construct a 'VEither' from a single error value.
--
-- Instead of:
--
-- >>> (VLeft (Vary.from @Bool True)) :: VEither '[Bool] String
-- VLeft (Vary.from @Bool True) 
--
-- You can just write:
--
-- >>> VEither.fromLeft @Bool True :: VEither '[Bool] String
-- VLeft (Vary.from @Bool True) 
fromLeft :: forall err errs a. err :| errs => err -> VEither errs a
{-# INLINE fromLeft #-}
fromLeft = Vary.from @err >>> VLeft

-- | Construct a 'VEither' from an @a@.
--
-- Exists for symmetry with 'fromLeft'.
-- Indeed, this is just another name for 'VRight' (and for 'pure').
fromRight :: forall a errs. a -> VEither errs a
{-# INLINE fromRight #-}
fromRight = VRight

-- | Case analysis on a 'VEither'. Similar to 'Data.Either.either'.
--
-- See also "VEither".'mapLeft', "VEither".'mapLeftOn' and "VEither".'mapRight'.
veither :: (Vary errs -> c) -> (a -> c) -> VEither errs a -> c
{-# INLINE veither #-}
veither f _ (VLeft x)     =  f x
veither _ g (VRight y)    =  g y

{-# COMPLETE VLeft, VRight #-}

-- Matches when the VEither contains one of the errors, returning @Vary errs@
pattern VLeft :: forall a errs. Vary errs -> VEither errs a
#if __GLASGOW_HASKELL__ >= 902
{-# INLINE VLeft #-}
#endif
pattern VLeft errs <- (toEither -> Left errs)
   where
      VLeft (Vary tag err) = VEither ((Vary (tag+1) err))

-- | Matches when the VEither contains the preferred value of type @a@.
pattern VRight :: forall a errs. a -> VEither errs a
#if __GLASGOW_HASKELL__ >= 902
{-# INLINE VRight #-}
#endif
pattern VRight a <- (toEither -> Right a)
  where
    VRight a = VEither (Vary.from @a a)

-- | Handle a particular error possibility.
--
-- Works very similarly to "Vary".'Vary.on'.
onLeft :: forall err b errs a. (err -> b) -> (VEither errs a -> b) -> VEither (err : errs) a -> b
{-# INLINE onLeft #-}
onLeft thiserrFun restfun ve = case ve of
  VLeft e -> Vary.on @err thiserrFun (\otherErr -> restfun (VLeft otherErr)) e
  VRight a -> restfun (VRight a)

-- | Handle the success posibility.
--
--
-- Works very similarly to "Vary".'Vary.on'.
-- Usually used together with "VError".'onLeft'.
onRight :: (a -> b) -> (VEither errs a -> b) -> VEither errs a -> b
{-# INLINE onRight #-}
onRight valfun restfun ve = case ve of
  VRight a -> valfun a
  VLeft err -> restfun (VLeft err)

-- | Handle a single error, by mapping it either to the success type @a@ or to one of the other errors in @errs@.
--
-- This is syntactic sugar over using "VEither".'onLeft',
-- but can be nicer to use if one or only a few error variants need to be handled,
-- because it lets you build a simple pipeline:
--
-- >>> :{
--  examplePipe ve = ve
--    & VEither.handle @Int (pure . show) 
--    & VEither.handle @Bool (pure . show)
-- :}
--
-- >>> :t examplePipe
-- examplePipe 
--   :: VEither (Int : Bool : errs) String -> VEither errs String
-- >>> examplePipe (VEither.fromLeft False :: VEither '[Int, Bool, Float] String)
-- VRight "False"
handle :: (err -> VEither errs a) -> VEither (err : errs) a -> VEither errs a
{-# INLINE handle #-}
handle fun = onLeft fun id

-- | If you have a VEither which does not actually contain any errors,
-- you can be sure it always contains an @a@.
--
-- Similar to "Vary".'Vary.intoOnly'.
intoOnly :: forall a. VEither '[] a -> a
{-# INLINE intoOnly #-}
intoOnly (VRight a) = a
intoOnly (VLeft emptyVary) = Vary.exhaustiveCase emptyVary


-- | Extend a smaller `VEiher` into a bigger one, change the order of its error types, or get rid of duplicate error types.
--
-- Similar to "Vary".'Vary.morph'
morph :: forall ys xs a. Subset (a : xs) (a : ys) => VEither xs a -> VEither ys a
{-# INLINE morph #-}
morph = toVary >>> Vary.morph >>> fromVary

-- | Execute a function expecting a larger (or differently-ordered) variant
-- with a smaller (or differently-ordered) variant,
-- by calling `morph` on it before running the function.
morphed :: forall xs ys a res. Subset (a : xs) (a : ys) => (VEither ys a -> res) -> VEither xs a -> res
{-# INLINE morphed #-}
morphed fun = fun . morph

-- | Map a function over one of the error values inside the 'VEither'.
--
-- Any other 'VLeft' and  also 'VRight' are kept untouched.
--
-- Similar to "Vary".'Vary.mapOn'.
mapLeftOn :: forall x y xs ys a. (Mappable x y xs ys) => (x -> y) -> VEither xs a -> VEither ys a
{-# INLINE mapLeftOn #-}
mapLeftOn _ (VRight val) = VRight val
mapLeftOn fun (VLeft err) = VLeft $ Vary.mapOn fun err

-- | Map a function over the 'VEither' if it contains a 'VLeft', otherwise leave it alone.
--
-- See also "VEither".'mapLeftOn', "VEither".'mapRight' and "VEither".'veither'.
--
mapLeft :: (Vary xs -> Vary ys) -> VEither xs a -> VEither ys a
{-# INLINE mapLeft #-}
mapLeft fun ve = case ve of 
    VRight a -> VRight a
    VLeft errs -> VLeft (fun errs)

-- | Map a function over the 'VEither' if it contains a 'VRight', otherwise leave it alone.
--
-- Exists for symmetry with "VEither".'mapLeft' and "VEither".'mapLeftOn'.
--
-- Indeed, it is just another name for 'fmap'.
--
-- See also "VEither".'veither'.
mapRight :: (x -> y) -> VEither errs x -> VEither errs y
{-# INLINE mapRight #-}
mapRight fun ve = case ve of 
    VRight a -> VRight (fun a)
    VLeft errs -> VLeft errs

instance (Show a, Show (Vary errs)) => Show (VEither errs a) where
  show (VLeft errs) = "VLeft (" <> show errs <> ")"
  show (VRight a) = "VRight " <> show a

instance (Eq a, Eq (Vary errs)) => Eq (VEither errs a) where
  a == b = toVary a == toVary b

instance (Ord a, Ord (Vary errs)) => Ord (VEither errs a) where
  compare a b = compare (toVary a) (toVary b)

instance (NFData a, NFData (Vary errs)) => NFData (VEither errs a) where
  rnf = toVary >>> rnf

instance Functor (VEither errs) where
  fmap :: forall a b. (a -> b) -> VEither errs a -> VEither errs b
  {-# INLINE fmap #-}
  fmap = mapRight

instance Applicative (VEither errs) where
  {-# INLINE pure #-}
  pure = VRight

  {-# INLINE (<*>) #-}
  (VRight fun) <*> (VRight val) = VRight (fun val)
  (VLeft err) <*> _ = (VLeft err)
  _ <*> (VLeft err) = (VLeft err)

instance Monad (VEither errs) where
  (>>=) :: forall a b. VEither errs a -> (a -> VEither errs b) -> VEither errs b
  (VRight a) >>= fun = fun a
  (VLeft err) >>= _  = (VLeft err)

instance Foldable (VEither errs) where
    foldMap _ (VLeft _) = mempty
    foldMap f (VRight y) = f y

    foldr _ z (VLeft _) = z
    foldr f z (VRight y) = f y z

    length (VLeft _)  = 0
    length (VRight _) = 1

instance Traversable (VEither errs) where
    traverse _ (VLeft x) = pure (VLeft x)
    traverse f (VRight y) = VRight <$> f y

instance Semigroup (VEither errs a) where
  (VRight a) <> _ = (VRight a)
  _ <> b = b

-- Look! A hand-written Generic instance! ;-)
--
-- This closely follows the implementation of the normal Either,
-- and pretends the type truly is built up of VLeft and VRight
instance Generic (VEither errs a) where
  type (Rep (VEither errs a)) =  D1
       (MetaData "VEither" "Vary.VEither" "vary" False)
       (C1
          (MetaCons "VLeft" PrefixI False)
          (S1
             (MetaSel
                Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
             (Rec0 (Vary errs)))
        :+: C1
              (MetaCons "VRight" PrefixI False)
              (S1
                 (MetaSel
                    Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
                 (Rec0 a)))

  from :: VEither errs a -> Rep (VEither errs a) x
  from ve =
    case ve of
      (VLeft err) -> M1 $ L1 $ M1 $ M1 $ K1 err
      (VRight val) -> M1 $ R1 $ M1 $ M1 $ K1 val

  to :: Rep (VEither errs a) x -> VEither errs a 
  to rep = case rep of
    (M1 (L1 (M1 (M1 (K1 err))))) -> VLeft err
    (M1 (R1 (M1 (M1 (K1 val))))) -> VRight val


-- Conceptually VEither is a Bifunctor,
-- but the kind does not align :-(
-- p has to be Type -> Type -> Type
-- But in the case of VEither it is [Type] -> Type -> Type
--
-- instance Bifunctor VEither where
--   first = mapLeft
--   second = mapRight
--   bimap = veither

#ifdef FLAG_HASHABLE 
instance (Hashable a, Hashable (Vary errs), (Eq (VEither errs a))) => Hashable (VEither errs a)
#endif

#ifdef FLAG_AESON 
deriving instance Aeson.ToJSON (Vary (a : errs)) => Aeson.ToJSON (VEither errs a)
deriving instance Aeson.FromJSON (Vary (a : errs)) => Aeson.FromJSON (VEither errs a)
#endif

#ifdef FLAG_QUICKCHECK
deriving instance (Arbitrary (Vary (a : errs))) => Test.QuickCheck.Arbitrary (VEither errs a)
#endif

#ifdef FLAG_CEREAL
deriving instance (Cereal.Serialize (Vary (a : errs))) => Cereal.Serialize (VEither errs a)
#endif

#ifdef FLAG_BINARY
deriving instance (Binary.Binary (Vary (a : errs))) => Binary.Binary (VEither errs a)
#endif
