{-# LANGUAGE TemplateHaskell #-}

module NoThunks.Internal.TH.Classes (
    eq
  , ord
  , foldable
  ) where

import Language.Haskell.TH

import NoThunks.Internal.TH.API

eq :: Q Type -> Instance
eq a = Instance {
      instanceHead = [t| Eq $a |]
    , instanceSigs = [
          ('(==), [t| $a -> $a -> Bool |])
        ]
    }

ord :: Q Type -> Instance
ord a = Instance {
      instanceHead = [t| Ord $a |]
    , instanceSigs = [
          ('compare, [t| $a -> $a -> Ordering |])
        ]
    }

foldable :: Q Type -> Instance
foldable t = Instance {
      instanceHead = [t| Foldable $t |]
    , instanceSigs = [
          ('foldr, [t| forall a b. (a -> b -> b) -> b -> $t a -> b |])
        ]
    }
