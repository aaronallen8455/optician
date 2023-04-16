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
import qualified Optician.TypeErrors as Err

solve :: Inputs -> P.TcPluginSolver
solve inputs _givens wanteds = do
  let wantedGenOptics = mapMaybe (isGenOptic inputs) wanteds
  tuples <- fmap catMaybes . for wantedGenOptics $ \(ct, args) -> do
    meResult <- buildOptic inputs (Ghc.ctLoc ct) args
    case meResult of
      Nothing -> pure Nothing
      Just (Left err) -> do
        newWanted <- Err.opticErrorToCt (Ghc.ctLoc ct) err
        pure $ Just
             ( (P.EvExpr . Ghc.Var $ Ghc.ctEvId newWanted, ct)
             , [newWanted]
             )
      Just (Right (expr, newWanteds)) ->
        pure $ Just
             ( (P.EvExpr expr, ct)
             , newWanteds
             )

  let (solved, newWanteds) = unzip tuples
  pure $ P.TcPluginOk solved (concat newWanteds)

-- | Identify wanteds for the GenOptic class
isGenOptic :: Inputs -> P.Ct -> Maybe (P.Ct, [P.Type])
isGenOptic inputs = \case
  ct@Ghc.CDictCan{..}
    | cc_class == genOpticClass inputs
    -> Just (ct, cc_tyargs)
  _ ->  Nothing

-- | Attemps to build an optic to satisfy a GenClass wanted.
-- If Nothing is emitted then the constraints attached to GenOptic will go
-- though the solver before the GenOptic constraint is retried.
buildOptic
  :: Inputs
  -> Ghc.CtLoc
  -> [P.Type]
  -> P.TcPluginM P.Solve (Maybe (Either Err.OpticErr (P.CoreExpr, [Ghc.Ct])))
buildOptic inputs ctLoc [ Ghc.LitTy (Ghc.StrTyLit labelArg)
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
  = Just <$> mkLens inputs ctLoc dataCon labelArg sTyArgs tTyArgs sArg tArg aArg bArg

  -- sum type
  | Just dataCons <- mDataCons
  , let matchFocusedCon = (== labelArg) . Ghc.occNameFS . Ghc.nameOccName . Ghc.getName
  , sTyCon == tTyCon -- fail so that the SameBase constraint will attempt to solve this equality
  = case List.partition matchFocusedCon dataCons of
      ([dataCon], otherDataCons) ->
        -- check type equalities
        if all (uncurry Ghc.eqType)
           $ prismTyEqPairs dataCon otherDataCons sTyArgs tTyArgs aArg bArg
           then Just <$> mkPrism inputs ctLoc dataCon tTyArgs sArg tArg aArg bArg
           else pure Nothing
      _ -> pure . Just . Left $ Err.TypeDoesNotHaveDataCon sArg labelArg

  where
    mDataCons = Ghc.tyConDataCons_maybe sTyCon
buildOptic _ _ _ = pure Nothing
