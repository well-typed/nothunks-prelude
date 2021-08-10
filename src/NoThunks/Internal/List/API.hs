{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module NoThunks.Internal.List.API (apiLazy, apiStrict) where

import Data.List

import Language.Haskell.TH hiding (Strict)

import qualified Data.Set as Set

import NoThunks.Internal.TH.API
import NoThunks.Internal.TH.Classes
import NoThunks.Strict

-- TODO: We should find a way to re-export functions we don't redefine
-- (like, Data.List exports a bunch of functions in terms of Foldable,
-- such as 'null', 'length', bunch of folds, bunch of special, folds..)
api :: Q Type -> API
api list = API {
      apiFunctions = [
          -- Basic functions
          ('(++)      , [t| forall a. $list a -> $list a -> $list a |])
        , ('head      , [t| forall a. $list a -> a |])
        , ('last      , [t| forall a. $list a -> a |])
        , ('tail      , [t| forall a. $list a -> $list a |])
        , ('init      , [t| forall a. $list a -> $list a |])
        , ('uncons    , [t| forall a. $list a -> Maybe (a, $list a) |])
#if MIN_VERSION_base(4,14,0)
        , ('singleton , [t| forall a. a -> $list a |])
#endif
          -- List transformations
        , ('map          , [t| forall a b. (a -> b) -> $list a -> $list b |])
        , ('reverse      , [t| forall a.   $list a -> $list a |])
        , ('intersperse  , [t| forall a.   a -> $list a -> $list a |])
          -- TODO: It's not obvious to me what the right signature for these are
--        , ('intercalate  , [t| forall a.   [a] -> [[a]] -> [a] |])
--        , ('transpose    , [t| forall a.   [[a]] -> [[a]] |])
        , ('subsequences , [t| forall a.   $list a -> [$list a] |])
        , ('permutations , [t| forall a.   $list a -> [$list a] |])
          -- Reducing lists (folds)
        , ('foldl1' , [t| forall a. (a -> a -> a) -> $list a -> a |])
          -- Special folds
        , ( 'concat    , [t| forall t a.   Foldable t => t ($list a) -> $list a |])
        , ( 'concatMap , [t| forall t a b. Foldable t => (a -> $list b) -> t a -> $list b|])
        ]
    , apiInstances = [
          do a <- newName "a"
             return ([a], [eq (varT a)], eq (list `appT` varT a))
        , do a <- newName "a"
             return ([a], [ord (varT a)], ord (list `appT` varT a))
        , do return ([], [], foldable list)
        ]
    , apiOverrides = Set.fromList [
          'concat
        ]
    }

apiLazy, apiStrict :: API
apiLazy   = api $ [t|        [] |]
apiStrict = api $ [t| Strict [] |]
