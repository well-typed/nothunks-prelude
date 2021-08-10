{-# LANGUAGE TemplateHaskell #-}

module Test.Sanity.API.List where

import NoThunks.Internal.TH.API
import NoThunks.Internal.List.API

import Data.List as Lazy

declareFunctions "Lazy" "Lazy" apiLazy
