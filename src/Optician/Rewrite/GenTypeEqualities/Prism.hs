module Optician.Rewrite.GenTypeEqualities.Prism
  ( prismTyEqPairs
  ) where

import qualified Optician.GhcFacade as Ghc

-- | Get the pairs of types that should be equal in order to construct a prism
prismTyEqPairs
  :: Ghc.DataCon -- focused
  -> [Ghc.DataCon] -- non-focused
  -> [Ghc.Type] -- s ty args
  -> [Ghc.Type] -- t ty args
  -> Ghc.Type -- a
  -> Ghc.Type -- b
  -> [(Ghc.Type, Ghc.Type)]
prismTyEqPairs dataCon otherDataCons sTyArgs tTyArgs aArg bArg =
  -- Ensure that it is safe to use 's' as 't' for any constructor besides the
  -- focused one (any ty vars that differ must only occur in the focused data con)
  let otherConEqs = do
        dc <- otherDataCons
        let sTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dc sTyArgs
            tTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dc tTyArgs
        zip sTys tTys

      sFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon sTyArgs
      sTupleTy = Ghc.mkBoxedTupleTy sFieldTys
      tFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon tTyArgs
      tTupleTy = Ghc.mkBoxedTupleTy tFieldTys

   in (aArg, sTupleTy)
    : (bArg, tTupleTy)
    : otherConEqs
