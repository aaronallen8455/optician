{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Optician.Solve
  ( solve
  ) where

import qualified Data.List as List
import           Data.Maybe
import           Data.Traversable
import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs (Inputs(..))
import           Optician.Rewrite.GenTypeEqualities.Lens (lensTyEqPairs)
import           Optician.Rewrite.GenTypeEqualities.Prism (prismTyEqPairs)
import           Optician.Solve.Lens (mkLens)
import           Optician.Solve.Prism (mkPrism)

solve :: Inputs -> P.TcPluginSolver
solve inputs _givens wanteds = do
  let wantedGenOptics = mapMaybe (isGenOptic inputs) wanteds
  solved <- for wantedGenOptics $ \(ct, args) ->
    (fmap . fmap) (, ct) (buildOptic inputs args)

  pure $ P.TcPluginOk (catMaybes solved) []

-- | Identify wanteds for the GenOptic class
isGenOptic :: Inputs -> P.Ct -> Maybe (P.Ct, [P.Type])
isGenOptic inputs = \case
  ct@Ghc.CDictCan{..}
    | cc_class == genOpticClass inputs
    -> Just (ct, cc_tyargs)
  _ ->  Nothing

-- | Attemps to build an optic to satisfy a GenClass wanted
buildOptic :: Inputs -> [P.Type] -> P.TcPluginM P.Solve (Maybe P.EvTerm)
buildOptic inputs [ Ghc.LitTy (Ghc.StrTyLit labelArg)
                  , sArg@(Ghc.TyConApp sTyCon sTyArgs)
                  , tArg@(Ghc.TyConApp tTyCon tTyArgs)
                  , aArg
                  , bArg
                  ]
  -- product type
  | Just [dataCon] <- mDataCons
  , sTyCon == tTyCon -- fail so that the SameBase constraint will attempt to solve this equality
  -- check type equalities
  , all (uncurry Ghc.eqType)
      $ lensTyEqPairs labelArg dataCon sTyArgs tTyArgs aArg bArg
  -- TODO allow existentials and contexts
  , null (Ghc.dataConExTyCoVars dataCon) -- no existentials allowed
  , null $ Ghc.dataConTheta dataCon ++ Ghc.dataConStupidTheta dataCon -- no contexts
  = fmap P.EvExpr <$> mkLens inputs dataCon tTyArgs labelArg sArg tArg aArg bArg

  -- sum type
  | Just dataCons <- mDataCons
  -- don't allow any data cons to have existentials or contexts
  -- TODO should be possible to allow them
  , all (null . Ghc.dataConExTyCoVars) dataCons -- no existentials allowed
  , all (null . Ghc.dataConTheta) dataCons -- no contexts
  , all (null . Ghc.dataConStupidTheta) dataCons
  -- check type equalities
  , let matchFocusedCon = (== labelArg) . Ghc.occNameFS . Ghc.nameOccName . Ghc.getName
  , ([dataCon], otherDataCons) <- List.partition matchFocusedCon dataCons
  , sTyCon == tTyCon -- fail so that the SameBase constraint will attempt to solve this equality
  , all (uncurry Ghc.eqType)
      $ prismTyEqPairs dataCon otherDataCons sTyArgs tTyArgs aArg bArg
  = fmap P.EvExpr <$>
      mkPrism inputs dataCon tTyArgs sArg tArg aArg bArg

  where
    mDataCons = Ghc.tyConDataCons_maybe sTyCon
buildOptic _ _ = pure Nothing
