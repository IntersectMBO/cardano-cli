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

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_HADDOCK not-home #-}

module Cardano.CLI.Vary.Core (Vary (..), pop) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Control.Exception (Exception (..))
import Data.Kind (Type)
import Data.Typeable (Typeable, typeOf)
import GHC.Exts (Any)
import GHC.Generics
import qualified Unsafe.Coerce as Data.Coerce

# ifdef FLAG_AESON
import qualified Data.Aeson as Aeson
# endif

# ifdef FLAG_HASHABLE
import Data.Hashable
# endif

# ifdef FLAG_QUICKCHECK
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (GSubterms, RecursivelyShrink)
# endif

# ifdef FLAG_BINARY
import qualified Data.Binary as Binary
# endif

# ifdef FLAG_CEREAL
import qualified Data.Serialize as Cereal
# endif

-- $setup
-- >>> :set -XDataKinds
-- >>> import Cardano.CLI.Vary (Vary, (:|))
-- >>> import qualified Vary

-- | Vary, contains one value out of a set of possibilities
--
-- Vary is what is known as a /Variant/ type.
-- This is also known as an /open union/ or /coproduct/, among other names.
--
-- You can see it as the generalization of `Either`.
-- Conceptually, these are the same:
--
-- > Vary [a, b, c, d, e]
-- > Either a (Either b (Either c (Either d e)))
--
-- However, compared to a deeply nested `Either`, `Vary` is:
--
-- - Much easier to work with;
-- - Much more efficient, as a single (strict) word is used for the tag.
--
-- `Vary`'s can be constructed with "Vary".`Vary.from` and values can be extracted using "Vary".`Vary.into` and "Vary".'Vary.on' .
data Vary (possibilities :: [Type]) = Vary {-# UNPACK #-} !Word Any

emptyVaryError :: forall anything. String -> Vary '[] -> anything
emptyVaryError name = error (name <> " was called on empty Vary '[]")

-- | Attempts to extract a value of the first type from the `Vary`.
--
-- If this failed, we know it has to be one of the other possibilities.
--
-- This function can also be seen as turning one layer of `Vary` into its isomorphic `Either` representation.
--
-- This function is not often useful in 'normal' code, but /super/ useful in generic code where you want to recurse on the variant's types.
--
-- For instance when implementing a typeclass for any `Vary` whose elements implement the typeclass:
--
--
-- > instance Show (Vary '[]) where
-- >    show = Vary.exhaustiveCase
-- >
-- > instance (Show a, Show (Vary as)) => Show (Vary (a : as)) where
-- >    show vary = case Vary.pop vary of
-- >        Right val -> "Vary.from " <> show val
-- >        Left other -> show other
--
-- To go the other way:
--
-- - Use "Vary".`Vary.morph` to turn @Vary as@ back into @Vary (a : as)@
-- - Use "Vary".`Vary.from` to turn @a@ back into @Vary (a : as)@
pop :: Vary (a : as) -> Either (Vary as) a
{-# INLINE pop #-}
pop (Vary 0 val) = Right (Data.Coerce.unsafeCoerce val)
pop (Vary tag inner) = Left (Vary (tag - 1) inner)

pushHead :: a -> Vary (a : as)
{-# INLINE pushHead #-}
pushHead val = Vary 0 (Data.Coerce.unsafeCoerce val)

{-# INLINE pushTail #-}
pushTail :: Vary as -> Vary (a : as)
pushTail (Vary tag inner) = Vary (tag + 1) inner

instance Eq (Vary '[]) where
  (==) = emptyVaryError "Eq.(==)"

instance (Eq a, Eq (Vary as)) => Eq (Vary (a : as)) where
  {-# INLINE (==) #-}
  a == b = pop a == pop b

instance Ord (Vary '[]) where
  compare = emptyVaryError "Ord.compare"

instance (Ord a, Ord (Vary as)) => Ord (Vary (a : as)) where
  {-# INLINE compare #-}
  l `compare` r = pop l `compare` pop r

instance Show (Vary '[]) where
  show = emptyVaryError "Show.show"

-- | `Vary`'s 'Show' instance only works for types which are 'Typeable'
--
-- This allows us to print the name of the type which
-- the current value is of.
--
-- >>> Vary.from @Bool True :: Vary '[Int, Bool, String]
-- Vary.from @Bool True
--
-- >>> Vary.from @(Maybe Int) (Just 1234) :: Vary '[Maybe Int, Bool]
-- Vary.from @(Maybe Int) (Just 1234)
instance (Typeable a, Show a, Show (Vary as)) => Show (Vary (a : as)) where
  showsPrec d vary = case pop vary of
    Right val ->
      showString "Vary.from "
        . showString "@"
        . showsPrec (d + 10) (typeOf val)
        . showString " "
        . showsPrec (d + 11) val
    Left other -> showsPrec d other

instance NFData (Vary '[]) where
  rnf = emptyVaryError "NFData.rnf"

instance (NFData a, NFData (Vary as)) => NFData (Vary (a : as)) where
  {-# INLINE rnf #-}
  rnf vary = rnf (pop vary)

instance (Typeable (Vary '[]), Show (Vary '[])) => Exception (Vary '[])

-- | See [Vary and Exceptions](#vary_and_exceptions) for more info.
instance (Exception e, Exception (Vary errs), Typeable errs) => Exception (Vary (e : errs)) where
  displayException vary =
    either displayException displayException (pop vary)

  toException vary =
    either toException toException (pop vary)

  fromException ex =
    (pushHead <$> fromException @e ex) <|> (pushTail <$> fromException @(Vary errs) ex)

-- case fromException @e some_exception of
--     Just e -> Just (pushHead e)
--     Nothing ->
--       case fromException @(Vary errs) some_exception of
--         Just vary -> Just (pushTail vary)
--         Nothing -> Nothing

-- Behold! A manually-written Generic instance!
--
-- This instance is very similar to the one for tuples (), (,), (,,), ...
-- but with each occurrence of :*: replaced by :+:
-- (and using `V1` instead of `U1` for the empty Vary)
type family RepHelper (list :: [Type]) :: Type -> Type where
  RepHelper '[] = V1
  RepHelper '[a] =
    S1
      ( MetaSel
          Nothing
          NoSourceUnpackedness
          NoSourceStrictness
          DecidedLazy
      )
      (K1 R a)
  RepHelper (a : b : bs) =
    S1
      (MetaSel Nothing NoSourceUnpackedness NoSourceStrictness DecidedLazy)
      (Rec0 a)
      :+: RepHelper (b : bs)

class GenericHelper (list :: [Type]) where
  fromHelper :: Vary list -> (RepHelper list) x
  toHelper :: (RepHelper list) x -> Vary list

instance GenericHelper '[] where
  fromHelper = emptyVaryError "Generic.from"
  toHelper void = case void of {}

instance GenericHelper '[a] where
  fromHelper vary = case pop vary of
    Right val -> M1 $ K1 $ val
    Left empty -> emptyVaryError "Generic.from" empty

  toHelper (M1 (K1 val)) = pushHead val

instance (GenericHelper (b : bs)) => GenericHelper (a : b : bs) where
  fromHelper vary = case pop vary of
    Right val -> L1 $ M1 $ K1 $ val
    Left rest -> R1 $ fromHelper rest

  toHelper (L1 (M1 (K1 val))) = pushHead val
  toHelper (R1 rest) = pushTail (toHelper rest)

-- | Vary '[] 's generic representation is `V1`.
instance Generic (Vary '[]) where
  type
    Rep (Vary '[]) =
      D1
        (MetaData "Vary" "Vary" "vary" False)
        (RepHelper '[])
  from = emptyVaryError "Generic.from"
  to void = case void of {}

-- | Any non-empty Vary's generic representation is encoded similar to a tuple but with `:+:` instead of `:*:`.
instance (GenericHelper (a : as)) => Generic (Vary (a : as)) where
  type
    Rep (Vary (a : as)) =
      D1
        (MetaData "Vary" "Vary" "vary" False)
        ( C1
            (MetaCons "from" PrefixI False)
            (RepHelper (a : as))
        )
  from vary = M1 $ M1 $ fromHelper vary
  to (M1 (M1 gval)) = toHelper gval

# ifdef FLAG_AESON
deriving instance Aeson.FromJSON (Vary '[])

deriving instance (Aeson.FromJSON a) => Aeson.FromJSON (Vary '[a])

-- | This instance round-trips iff there is no overlap between the encodings of the element types.
--
-- For example, a `Vary '[Int, String] is round-trippable
-- but a `Vary '[String, Char]` is not.
instance (Aeson.FromJSON a, Aeson.FromJSON (Vary (b : bs))) => Aeson.FromJSON (Vary (a : b : bs)) where
  {-# INLINE parseJSON #-}
  parseJSON val = (pushHead <$> Aeson.parseJSON val) <|> (pushTail <$> Aeson.parseJSON val)

deriving instance Aeson.ToJSON (Vary '[])

deriving instance (Aeson.ToJSON a) => Aeson.ToJSON (Vary '[a])

-- | This instance round-trips iff there is no overlap between the encodings of the element types.
--
-- For example, a `Vary '[Int, String] is round-trippable
-- but a `Vary '[String, Char]` is not.
instance (Aeson.ToJSON a, Aeson.ToJSON (Vary (b : bs))) => Aeson.ToJSON (Vary (a : b : bs)) where
  {-# INLINE toJSON #-}
  toJSON vary =
    either Aeson.toJSON Aeson.toJSON (pop vary)

  {-# INLINE toEncoding #-}
  toEncoding vary =
    either Aeson.toEncoding Aeson.toEncoding (pop vary)
# endif

# ifdef FLAG_QUICKCHECK
instance (Test.QuickCheck.Arbitrary a) => Test.QuickCheck.Arbitrary (Vary '[a]) where
  arbitrary = pushHead <$> arbitrary
  shrink = genericShrink

instance
  ( Arbitrary a,
    Arbitrary (Vary (b : bs)),
    Generic (Vary (a : b : bs)),
    RecursivelyShrink (Rep (Vary (a : b : bs))),
    GSubterms (Rep (Vary (a : b : bs))) (Vary (a : b : bs))
  ) =>
  Test.QuickCheck.Arbitrary (Vary (a : b : bs))
  where
  arbitrary = oneof [pushHead <$> arbitrary, pushTail <$> arbitrary]
  shrink = genericShrink
# endif

#ifdef FLAG_HASHABLE
class FastHashable a where
  badHashWithSalt :: Int -> a -> Int

instance (Hashable a) => FastHashable (Vary '[a]) where
  {-# INLINE badHashWithSalt #-}
  badHashWithSalt salt vary = case pop vary of
    Right val -> hashWithSalt salt val
    Left empty -> emptyVaryError "hashWithSalt" empty

instance (Hashable a, FastHashable (Vary (b : bs))) => FastHashable (Vary (a : b : bs)) where
  {-# INLINE badHashWithSalt #-}
  badHashWithSalt salt vary = case pop vary of
    Right val -> hashWithSalt salt val
    Left rest -> badHashWithSalt salt rest

instance
  ( Eq (Vary (a : as)),
    FastHashable (Vary (a : as))
  ) =>
  Hashable (Vary (a : as))
  where
  hashWithSalt salt vary@(Vary tag _inner) = fromIntegral tag `hashWithSalt` badHashWithSalt salt vary
  hash vary@(Vary tag _inner) = badHashWithSalt (fromIntegral tag) vary
#endif

#ifdef FLAG_BINARY
class BinaryHelper a where
  binaryPutVariant :: a -> Binary.Put
  binaryGetVariant :: Word -> Binary.Get a

instance BinaryHelper (Vary '[]) where
  {-# INLINE binaryPutVariant #-}
  binaryPutVariant emptyVary = case from emptyVary of {}

  {-# INLINE binaryGetVariant #-}
  binaryGetVariant = emptyVaryError "binaryGetVariant" undefined

instance
  ( Binary.Binary a
  , BinaryHelper (Vary as)
  ) =>
  BinaryHelper (Vary (a : as))
  where
  {-# INLINE binaryPutVariant #-}
  binaryPutVariant vary = case pop vary of
    Right val -> Binary.put val
    Left val -> binaryPutVariant val

  {-# INLINE binaryGetVariant #-}
  binaryGetVariant 0 = pushHead <$> Binary.get @a
  binaryGetVariant n = pushTail <$> binaryGetVariant @(Vary as) (n - 1)

instance (BinaryHelper (Vary as)) => Binary.Binary (Vary as) where
  {-# INLINE put #-}
  put vary@(Vary n _) = do
    Binary.put n
    binaryPutVariant vary
  {-# INLINE get #-}
  get = do
    tag <- Binary.get
    binaryGetVariant tag
#endif

#ifdef FLAG_CEREAL
class SerializeHelper a where
  cerealPutVariant :: a -> Cereal.Put
  cerealGetVariant :: Word -> Cereal.Get a

instance SerializeHelper (Vary '[]) where
  {-# INLINE cerealPutVariant #-}
  cerealPutVariant emptyVary = case from emptyVary of {} 
  {-# INLINE cerealGetVariant #-}
  cerealGetVariant = emptyVaryError "cerealGetVariant" undefined

instance
  ( Cereal.Serialize a
  , SerializeHelper (Vary as)
  ) =>
  SerializeHelper (Vary (a : as))
  where
  {-# INLINE cerealPutVariant #-}
  cerealPutVariant vary = case pop vary of
    Right val -> Cereal.put val
    Left val -> cerealPutVariant val

  {-# INLINE cerealGetVariant #-}
  cerealGetVariant 0 = pushHead <$> Cereal.get @a
  cerealGetVariant n = pushTail <$> cerealGetVariant @(Vary as) (n - 1)

instance (SerializeHelper (Vary as)) => Cereal.Serialize (Vary as) where
  {-# INLINE put #-}
  put vary@(Vary n _) = do
    Cereal.putWord64le (fromIntegral n)
    cerealPutVariant vary
  {-# INLINE get #-}
  get = do
    tag <- Cereal.getWord64le
    cerealGetVariant (fromIntegral tag)
#endif
