module Optician.Rewrite.GenTypeEqualities.Prism
  ( prismTyEqPairs
  ) where

import           Control.Monad (guard)

import qualified Optician.GhcFacade as Ghc

-- | Get the pairs of types that should be equal in order to construct a prism
prismTyEqPairs
  :: Ghc.TyCon
  -> Ghc.DataCon -- focused
  -> [Ghc.DataCon] -- non-focused
  -> [Ghc.Type] -- s ty args
  -> [Ghc.Type] -- t ty args
  -> Ghc.Type -- a
  -> Ghc.Type -- b
  -> [(Ghc.Type, Ghc.Type)]
prismTyEqPairs tyCon dataCon otherDataCons sTyArgs tTyArgs aArg bArg = do
  -- Don't generate constraints if the data con has existentials because they
  -- can cause insolubles with variables of unknown origin. The user should
  -- instead get the custom error thrown from the solver.
  guard (null $ Ghc.dataConExTyCoVars dataCon)

  -- Ensure that it is safe to use 's' as 't' for any constructor besides the
  -- focused one (any ty vars that differ must only occur in the focused data con)
  let otherConEqs = do
        dc <- otherDataCons
        let exTys = Ghc.mkTyVarTy <$> Ghc.dataConExTyCoVars dc
            sTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dc (sTyArgs ++ exTys)
            tTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dc (tTyArgs ++ exTys)
        zip sTys tTys

      existTys = Ghc.mkTyVarTy <$> Ghc.dataConExTyCoVars dataCon
      sFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon (sTyArgs ++ existTys)
      sTupleTy = Ghc.mkBoxedTupleTy sFieldTys
      tFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon (tTyArgs ++ existTys)
      tTupleTy = Ghc.mkBoxedTupleTy tFieldTys
      roles = Ghc.tyConRoles tyCon
      phantomTyEqs = do
        (sTyArg, tTyArg, Ghc.Phantom) <- zip3 sTyArgs tTyArgs roles
        [(sTyArg, tTyArg)]

  (aArg, sTupleTy)
    : (bArg, tTupleTy)
    : otherConEqs
    ++ phantomTyEqs
