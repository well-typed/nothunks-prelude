{-# LANGUAGE TemplateHaskell #-}

module NoThunks.StateT where

import NoThunks.Internal.TH.API
import NoThunks.Internal.StateT.API
import NoThunks.Internal.StateT.Strict as Strict

import Control.Monad.Trans.State.Strict as Lazy

declareFunctions "Lazy" "Strict" apiStrict
