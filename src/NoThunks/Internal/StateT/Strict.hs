module NoThunks.Internal.StateT.Strict (
    Strict(..)
    -- * Conversions
  , fromStrictStateT
  ) where

import NoThunks.Strict

import Data.Coerce

import Control.Monad.Trans.State.Strict as Lazy

newtype instance Strict StateT s m a = StrictStateT (StateT s m a)

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

fromStrictStateT :: Strict StateT s m a -> StateT s m a
fromStrictStateT = coerce
