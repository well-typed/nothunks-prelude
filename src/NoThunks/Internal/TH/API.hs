{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module NoThunks.Internal.TH.API (
    API(..)
  , Instance(..)
    -- * Opening the API
  , declareFunctions
  , declareInstances
  ) where

import Control.Exception (assert)
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Datatype (TypeSubstitution(..))

data API = API {
      apiFunctions :: [(Name, Q Type)]

      -- We explicitly list the universally quantified type variables
      -- of the instance
    , apiInstances :: [Q ([Name], [Instance], Instance)]

      -- Functions that need an override
    , apiOverrides :: Set Name
    }

data Instance = Instance {
      instanceHead :: Q Type
    , instanceSigs :: [(Name, Q Type)]
    }

type Scope = String

instance TypeSubstitution Instance where
  applySubstitution s Instance{instanceHead, instanceSigs} = Instance{
        instanceHead = fmap (applySubstitution s) instanceHead
      , instanceSigs = map (second (fmap (applySubstitution s))) instanceSigs
      }

  freeVariables = error "not defined" -- not definable due to use of @Q@

{-------------------------------------------------------------------------------
  Opening the scope
-------------------------------------------------------------------------------}

-- TODO: Declare INLINE whatever possible.

declareFunctions :: Scope -> Scope -> API -> Q [Dec]
declareFunctions scopeLazy scopeStrict API{apiFunctions, apiOverrides} =
    concat <$> mapM (uncurry go) apiFunctions
  where
    go :: Name -> Q Type -> Q [Dec]
    go f typ' = do
        typ <- typ'
        sequence [
            sigD local (return typ)
          , funD local [clause [] (normalB (body typ)) []]
          ]
      where
        local, qualLazy, qualStrict :: Name
        (local, qualLazy, qualStrict) =
            case f of
              Name f' _flavour -> (
                  Name f' NameS
                , Name f' (NameQ (ModName scopeLazy))
                , Name f' (NameQ (ModName scopeStrict))
                )

        body :: Type -> Q Exp
        body typ =
            if f `Set.member` apiOverrides
              then varE qualStrict
              else qualLazy `coerceAt` return typ

-- TODO: This works, but we now use a different way to coerce for instances
-- than we do for normal functions. We should try to make this more uniform.
declareInstances :: API -> API -> Q [Dec]
declareInstances = \defaults api ->
    mapM (uncurry go) (zip (apiInstances defaults) (apiInstances api))
  where
    go :: Q ([Name], [Instance], Instance)
       -> Q ([Name], [Instance], Instance)
       -> Q Dec
    go defQ thisQ = do
        (defVars,  _defCtxt, defInst') <- defQ
        (thisVars, thisCtxt, thisInst) <- thisQ
        let subst   = Map.fromList $ zipWith (\d t -> (d, VarT t)) defVars thisVars
            defInst = applySubstitution subst defInst'
        instanceD'
          (cxt (map instanceHead thisCtxt))
          (instanceHead thisInst)
          (zipWith goF (instanceSigs defInst) (instanceSigs thisInst))

    goF :: (Name, Q Type) -> (Name, Q Type) -> Q [Dec]
    goF (defName, defType') (thisName, thisType') = do
        defType  <- defType'
        thisType <- thisType'

        sequence [
            sigD thisName (return thisType)
          , funD thisName [clause [] (normalB (body defType thisType)) []]
          ]
      where
        body :: Type -> Type -> Q Exp
        body def this =
                  varE 'coerce
          `appE` (varE defName `sigE` return (def `instantiatedTo` this))

coerceAt :: Name -> Q Type -> Q Exp
coerceAt n = (>>= appE (varE 'coerce) . aux)
  where
    aux :: Type -> Q Exp
    aux (ForallT ts _ _) = appTypeEs (varE n) (map (varT . refTyVarName) ts)
    aux _otherwise       = varE n

instantiatedTo :: Type -> Type -> Type
def `instantiatedTo` this =
    case (def, this) of
      (ForallT xs _cxt def', ForallT ys _ _) ->
        assert (length xs == length ys) $
        let subst :: Map Name Type
            subst = Map.fromList $ zipWith replace xs ys
        in applySubstitution subst def'
      _otherwise ->
        def
  where
#if MIN_VERSION_template_haskell(2,17,0)
    replace :: TyVarBndr flag -> TyVarBndr flag' -> (Name, Type)
#else
    replace :: TyVarBndr -> TyVarBndr -> (Name, Type)
#endif
    replace x y = (refTyVarName x, VarT (refTyVarName y))

{-------------------------------------------------------------------------------
  Auxiliary TH
-------------------------------------------------------------------------------}

appTypeEs :: Q Exp -> [Q Type] -> Q Exp
appTypeEs = foldl appTypeE

#if MIN_VERSION_template_haskell(2,17,0)
refTyVarName :: TyVarBndr flag -> Name
refTyVarName (PlainTV  n _)   = n
refTyVarName (KindedTV n _ _) = n
#else
refTyVarName :: TyVarBndr -> Name
refTyVarName (PlainTV  n)   = n
refTyVarName (KindedTV n _) = n
#endif

-- funT :: Q Type -> Q Type -> Q Type
-- funT a b = appT (appT arrowT a) b

-- | Variation on 'instanceD' that allows for multiple declarations per
-- function in the class (useful for a signature/implementation pair)
instanceD' :: Q Cxt -> Q Type -> [Q [Dec]] -> Q Dec
instanceD' qCxt qTyp qDecs =
    InstanceD Nothing <$> qCxt <*> qTyp <*> (concat <$> sequence qDecs)
