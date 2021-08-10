{-# LANGUAGE TemplateHaskell #-}

module NoThunks.Internal.StateT.API (apiLazy, apiStrict) where

import Control.Monad.Trans.State.Strict
import Language.Haskell.TH hiding (Strict)

import qualified Data.Set as Set

import NoThunks.Internal.TH.API
import NoThunks.Strict

api :: Q Type -> API
api stateT = API {
      apiFunctions = [
          ('runStateT, [t| forall s m a. $stateT s m a -> s -> m (a, s) |] )
        ]
    , apiInstances = []
    , apiOverrides = Set.empty
    }

apiLazy, apiStrict :: API
apiLazy    = api $ [t|        StateT |]
apiStrict  = api $ [t| Strict StateT |]
