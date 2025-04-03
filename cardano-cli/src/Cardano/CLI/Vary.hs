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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Cardano.CLI.Vary
  ( -- * General Usage
    -- $setup
    -- $motivating_example
    -- $vary_and_exceptions
    -- $vary_and_serialization

    -- * Core type definition
    Vary
  , (:|)

    -- * Construction and Destruction:
  , from
  , into
  , intoOnly

    -- * case analysis ("pattern matching"):

    -- |
    -- Vary does not support traditional pattern matching,
    -- because GHC is not able to check them for exhaustiveness.
    --
    -- Instead, Vary supports the next best thing: building up a pattern match using the 'on' combinator.
  , on
  , exhaustiveCase
  , defaultCase
  , pop

    -- * Transforming
  , mapOn
  , morph
  , morphed
  )
where

import Cardano.CLI.Vary.Core (Vary (..))
import Cardano.CLI.Vary.Utils

import Control.Monad (guard)
import Data.Kind
import GHC.TypeLits

import Unsafe.Coerce (unsafeCoerce)

-- $setup
--
-- == Setup
--
-- This module is intended to be used qualified:
--
-- >>> import Cardano.CLI.Vary (Vary, (:|))
-- >>> import qualified Vary
--
-- You probably often want to use it together with the "Vary.VEither" module:
--
-- >>> import Cardano.CLI.Vary.VEither (VEither(VLeft, VRight))
-- >>> import qualified Vary.VEither as VEither
--
-- And for many functions, it is useful (and sometimes outright necessary) to enable the following extensions:
--
-- >>> :set -XDataKinds
--
-- Finally, some example snippets in this module make use of 'Data.Function.&', the left-to-right function application operator.
--
-- >>> import Data.Function ((&))

-- $motivating_example
--
--  == Motivating Example
--
-- A longer example on why you would want to use Vary [can be found in the package README on GitHub](https://github.com/qqwy/haskell-vary#readme)

-- $vary_and_exceptions
--
-- == Vary and Exceptions #vary_and_exceptions#
--
-- 'Vary' implements 'Control.Exception.Exception',
-- and is an /excellent/ type to use with 'Control.Exception.throw' and 'Control.Exception.catch'.
--
-- >>> import Control.Exception
-- >>> no_xyzzy  = Vary.from (NoMethodError "xyzzy") :: Vary '[NoMethodError, ArithException]
-- >>> divby0    = Vary.from DivideByZero            :: Vary '[NoMethodError, ArithException]
--
-- >>> throw no_xyzzy `catch` \(e :: Vary '[NoMethodError, ArithException]) -> putStrLn ("Caught: `" <> show e <> "`")
-- Caught: `Vary.from @NoMethodError xyzzy`
--
-- === Catching individual errors of a thrown 'Vary'
--
-- 'Control.Exception.toException' is implemented to throw the particular /internal/ type.
--
-- This means that you can catch any of the particular individual possibilities of a thrown Vary if you like,
-- and have the others bubble up:
--
-- >>> throw no_xyzzy `catch` \(e :: NoMethodError) -> putStrLn ("Caught: `" <> show e <> "`")
-- Caught: `xyzzy`
--
-- >>> throw divby0 `catch` \(e :: NoMethodError) -> putStrLn ("Caught: `" <> show e <> "`")
-- *** Exception: divide by zero
--
-- === Catching groups of (individually thrown) errors
--
-- Also, 'Control.Exception.fromException' is implemented to /match/ any of the contained possibilities:
--
-- >>> catcher inner = inner `catch` \(e :: Vary '[NoMethodError, ArithException]) -> putStrLn ("Caught: `" <> show e <> "`")
--
-- So not only is the following exception caught:
--
-- >>> vary = Vary.from (NoMethodError "plover") :: Vary '[NoMethodError, ArithException]
-- >>> catcher (throw vary)
-- Caught: `Vary.from @NoMethodError plover`
--
-- But it will also catch a thrown @ArithException@
--
-- >>> catcher (throw DivideByZero)
-- Caught: `Vary.from @ArithException divide by zero`
--
-- or a thrown @NoMethodError@!
--
-- >>> catcher (throw (NoMethodError "plugh"))
-- Caught: `Vary.from @NoMethodError plugh`
--
-- /(and other exceptions of course still bubble up)/
--
-- >>> catcher (throw AllocationLimitExceeded)
-- *** Exception: allocation limit exceeded

-- $vary_and_serialization
--
-- == (De)Serializing Vary values
--
-- `Vary` has optional dependencies to enable `aeson`'s `Data.Aeson`, `binary`'s `Data.Binary` and `cereal`'s `Data.Serealize` serialization.
--
-- Specifically for Aeson serialization, Vary datatypes are encoded
-- as their ['UntaggedValue'](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson-Types.html#t:SumEncoding) encoding.
-- This means that serialization to JSON only round-trips when the encodings are disjoint;
-- on decoding, the first variant to succeed is used.
--
-- The Binary and Serialize instances always round-trip, as their encoding contains the variant's tag index.

-- | Builds a Vary from the given value.
--
-- >>> let thingy :: Vary [Bool, Char]; thingy = Vary.from 'a'
-- >>> thingy
-- Vary.from @Char 'a'
--
-- In the case of number literals or (with OverloadedStrings or OverloadedLists enabled) string or list literals,
-- it might be necessary to include a TypeApplication.
-- In most other cases, GHC is able to infer which possibility to use (though you might still like type applications even here for improved readability).
--
-- >>> Vary.from @Int 42 :: Vary [Int, String]
-- Vary.from @Int 42
--
-- In the case of the Vary contains duplicate types,
-- the first matching type index is used.
from
  :: forall a l. a :| l => a -> Vary l
{-# INLINE from #-}
from = fromAt @(IndexOf a l)

-- | Attempts to turn the Vary back into a particular type.
--
-- This might fail since the Vary might actually contain another possibility,
-- which is why a `Maybe` is returned.
--
-- If you have a single possibility, you can use `intoOnly` instead.
--
-- == Polymorphic functions
--
-- If you pass the result to a polymorphic function, GHC might not be able to infer which result type you'd like to try to extract.
-- Indicate the desired result type using a TypeApplication:
--
-- >>> let vary = Vary.from @Bool True :: Vary [Bool, String]
-- >>> Vary.into @Bool vary
-- Just True
--
-- == Type errors
-- Sometimes you might see nasty long type errors, containing the string
-- `Type_List_Too_Vague___Please_Specify_Prefix_Of_List_Including_The_Desired_Type's_Location`.
--
-- This happens when other parts of your code keep the type list fully abstract (only use the `:|` constraint).
--
-- You can fix it by either giving a type to an intermediate value,
-- or by passing a second type application to this function:
--
-- >>> let vary = if True then Vary.from True else Vary.from 'a' -- Inferred type: `Bool :| l, Char :| l => Vary l`
-- >>> Vary.into @Bool @(Char : Bool : _) vary
-- Just True
--
-- As you can see from the above example, it is often not necessary to specify the /full/ type list.
-- A prefix is commonly enough.
into :: forall a l. a :| l => Vary l -> Maybe a
{-# INLINE into #-}
into = intoAt @(IndexOf a l)

-- | Extract the value of a variant with one possibility.
--
-- A variant with only a single possibility
-- can always be safely turned back into this one type.
--
-- If you have multiple possibilities, use `into`.
intoOnly :: forall a. Vary '[a] -> a
{-# INLINE intoOnly #-}
intoOnly (Vary _ val) = unsafeCoerce val

-- | Base case of an exhaustive pattern match.
--
-- Use it together with `on`,
-- or whenever you have an empty `Vary '[]` that you need to get rid of.
-- (Like in a recursive typeclass definition. See "Vary".'pop')
--
-- Since it is impossible to actually /construct/ a value of the type @Vary '[]@,
-- we can "turn it into anything", just like `Data.Void.absurd`.
exhaustiveCase :: forall anything. Vary '[] -> anything
{-# INLINE exhaustiveCase #-}
exhaustiveCase _vary =
  error
    "Somehow someone got their hands on a runtime value of type Vary '[]. This should be impossible, so someone did a bad unsafeCoerce somewhere!"

-- | Base case of a non-exhaustive pattern match. Use it together with `on`.
--
-- If you've handled the variants you like and have some left,
-- you can specify a default fallback value using `defaultCase`.
--
-- Indeed, this function is just another name for `const`.
defaultCase :: forall a l. a -> Vary l -> a
{-# INLINE defaultCase #-}
defaultCase = const

-- | Extend a smaller `Vary` into a bigger one, change the order of its elements, or get rid of duplicates.
--
-- === Extend a smaller `Vary`:
-- >>> small = Vary.from True :: Vary '[Bool]
-- >>> big = Vary.morph small :: Vary [Bool, Int, String]
-- >>> big
-- Vary.from @Bool True
--
-- === Reorder elements:
-- >>> boolfirst = Vary.from @Int 42   :: Vary [Bool, Int]
-- >>> intfirst = Vary.morph boolfirst :: Vary [Int, Bool]
-- >>> intfirst
-- Vary.from @Int 42
--
-- === Get rid of duplicate elements:
-- >>> duplicates = Vary.from @Int 69       :: Vary [Int, Int, Int]
-- >>> noduplicates = Vary.morph duplicates :: Vary '[Int]
-- >>> noduplicates
-- Vary.from @Int 69
--
-- === Type applications
-- Morph intentionally takes the result type list as first type-application parameter.
-- This allows you to write above examples in this more concise style instead:
--
-- >>> big = Vary.morph @[Bool, Int, String] small
-- >>> intfirst = Vary.morph @[Int, Bool] boolfirst
-- >>> noduplicates = Vary.morph @'[Int] duplicates
--
--
-- == Efficiency
-- This is a O(1) operation, as the tag number stored in the variant is
-- changed to the new tag number.
--
-- In many cases GHC can even look through the old->new Variant structure entirely,
-- and e.g. inline the variant construction all-together.
morph :: forall ys xs. Subset xs ys => Vary xs -> Vary ys
morph = morph' @xs @ys

fromAt
  :: forall (n :: Nat) (l :: [Type])
   . KnownNat n
  => Index n l
  -> Vary l
{-# INLINE fromAt #-}
fromAt a = Vary (natValue @n) (unsafeCoerce a)

intoAt
  :: forall (n :: Nat) (l :: [Type])
   . KnownNat n
  => Vary l
  -> Maybe (Index n l)
{-# INLINE intoAt #-}
intoAt (Vary t a) = do
  guard (t == natValue @n)
  return (unsafeCoerce a)

-- | Handle a particular variant possibility.
--
-- This is the main way to do case analysis (or 'deconstruct') a variant.
--
-- Use it together with `exhaustiveCase` if you handle all possibilities,
-- or `defaultCase` if you don't want to.
--
-- Even though in many cases GHC is able to infer the types,
-- it is a good idea to combine it with `TypeApplications`:
--
-- Note that by doing so, GHC can infer the type of the function without problems:
--
-- >>> :{
--   example vary =
--     vary &
--     ( Vary.on @Bool show
--     $ Vary.on @Int (\x -> show (x + 1))
--     $ Vary.defaultCase "other value"
--     )
-- :}
--
-- >>> :t example
-- example :: Vary (Bool : Int : l) -> String
on :: forall a b l. (a -> b) -> (Vary l -> b) -> Vary (a : l) -> b
{-# INLINE on #-}
on thisFun restFun vary =
  case into @a vary of
    Just val -> thisFun val
    Nothing ->
      restFun (coerceHigher vary)
 where
  -- Invariant: does not contain @a
  {-# INLINE coerceHigher #-}
  coerceHigher :: Vary (a : l) -> Vary l
  coerceHigher (Vary idx val) =
    unsafeCoerce (Vary (idx - 1) val)

-- | Execute a function expecting a larger (or differently-ordered) variant
-- with a smaller (or differently-ordered) variant,
-- by calling `morph` on it before running the function.
morphed :: forall a b res. Subset a b => (Vary b -> res) -> Vary a -> res
{-# INLINE morphed #-}
morphed fun = fun . morph

-- | Run a function on one of the variant's possibilities, keeping all other possibilities the same.
--
-- This is the generalization of functions like Either's `Data.Either.Extra.mapLeft` and `Data.Either.Extra.mapRight`.
--
-- If you want to map a polymorphic function like `show` which could match more than one possibility,
-- use a TypeApplication to specify the desired possibility to match:
--
-- >>> :{
-- (Vary.from @Int 42           :: Vary [Int, Bool] )
--   & Vary.mapOn @Bool show    -- Vary [Int, String]
--   & Vary.mapOn @Int show     -- Vary [String, String]
-- :}
-- Vary.from @[Char] "42"
--
-- If you end up with a variant with multiple duplicate possibilities, use `morph` to join them:
--
-- >>> :{
-- (Vary.from True                :: Vary [Char, Int, Bool])
--   & Vary.mapOn @Bool show      -- Vary [Char, Int, String]
--   & Vary.mapOn @Int show       -- Vary [Char, String, String]
--   & Vary.mapOn @Char show      -- Vary [String, String, String]
--   & Vary.morph @'[String]       -- Vary '[String]
--   & Vary.intoOnly              -- String
-- :}
-- "True"

-- Note that if you end up handling all cases of a variant, you might prefer using `Vary.on` and `Vary.exhaustiveCase` instead.
--
-- == Generic code
--
-- It is possible to use the most general type of this function in your own signatures;
-- To do this, add the `Mappable` constraint (exposed from `Vary.Utils`)
-- to relate the input variant with the output variant.
--
-- >>> import qualified Data.Char
-- >>> :{
-- example4 :: (Vary.Utils.Mappable Int Bool xs ys, Vary.Utils.Mappable Char Int ys zs) => Vary xs -> Vary zs
-- example4 vary =
--   vary
--   & Vary.mapOn @Int (\x -> x > 0)
--   & Vary.mapOn @Char Data.Char.ord
-- :}
--
-- == Duplicate possibilities
-- Vary.mapOn will only work on the first instance of the type that is encountered.
-- This is only a problem if a possibility is in the list multiple times;
-- be sure to `Vary.morph` duplicate possibilities away if needed.
mapOn :: forall a b xs ys. Mappable a b xs ys => (a -> b) -> Vary xs -> Vary ys
mapOn fun vary@(Vary tag val) =
  case into @a vary of
    Just a -> from @b (fun a)
    Nothing -> (Vary tag val)
