module NoThunks.Internal.List.Strict (
    Strict(..)
    -- * Conversion
  , toStrictList
  , fromStrictList
    -- * Pattern synonyms
  , pattern Nil
  , pattern Cons
    -- * Overrides
  , concat
  --, foldr
  ) where

import Prelude hiding (
    concat
  , foldr
  )

import Data.Coerce

import NoThunks.Strict

import qualified Data.Foldable as Foldable

newtype instance Strict [] a = UnsafeStrictList [a]

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

toStrictList :: [a] -> Strict [] a
toStrictList = Foldable.foldr Cons Nil

fromStrictList :: Strict [] a -> [a]
fromStrictList = coerce

{-------------------------------------------------------------------------------
  Pattern synonyms
-------------------------------------------------------------------------------}

{-# COMPLETE Nil, Cons #-}

pattern Nil :: Strict [] a
pattern Nil = UnsafeStrictList []

pattern Cons :: a -> Strict [] a -> Strict [] a
pattern Cons x xs <- (uncons -> Just (x, xs))
  where
    Cons !x !(UnsafeStrictList xs) = UnsafeStrictList (x : xs)

{-------------------------------------------------------------------------------
  Overrides
-------------------------------------------------------------------------------}

concat :: Foldable t => t (Strict [] a) -> Strict [] a
concat = go . Foldable.toList
  where
    go :: [Strict [] a] -> Strict [] a
    go []                = Nil
    go (Nil       : xss) = go xss
    go (Cons x xs : xss) = Cons x (go (xs : xss))

{-
foldr :: forall a b. (a -> b -> b) -> b -> Strict [a] -> b
foldr op e = go
  where
    go :: Strict [a] -> b
    go Nil         = e
    go (Cons x xs) = x `op` go xs
-}

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

uncons :: Strict [] a -> Maybe (a, Strict [] a)
uncons (UnsafeStrictList [])     = Nothing
uncons (UnsafeStrictList (x:xs)) = Just (x, UnsafeStrictList xs)
