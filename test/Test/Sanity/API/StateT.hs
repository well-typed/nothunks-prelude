{-# LANGUAGE TemplateHaskell #-}

module Test.Sanity.API.StateT where

import NoThunks.Internal.TH.API
import NoThunks.Internal.StateT.API

import Control.Monad.Trans.State.Strict as Lazy

declareFunctions "Lazy" "Lazy" apiLazy
