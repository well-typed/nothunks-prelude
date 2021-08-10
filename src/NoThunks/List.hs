{-# LANGUAGE TemplateHaskell #-}

module NoThunks.List where

import NoThunks.Internal.TH.API
import NoThunks.Internal.List.API
import NoThunks.Internal.List.Strict as Strict

import Data.List as Lazy

declareFunctions "Lazy" "Strict" apiStrict
declareInstances apiLazy apiStrict
