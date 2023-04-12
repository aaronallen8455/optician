{-# LANGUAGE DataKinds #-}

module Optician.Rewrite.GenTypeEqualities.Lens
  ( lensTyEqPairs
  ) where

import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc

-- | The type pairs that must be equal in order to construct a lens
lensTyEqPairs
  :: P.FastString
  -> P.DataCon
  -> [P.Type]
  -> [P.Type]
  -> P.Type
  -> P.Type
  -> [(P.Type, P.Type)]
lensTyEqPairs fieldName dataCon sTyArgs tTyArgs aTy bTy = do
  let existTys = Ghc.mkTyVarTy <$> Ghc.dataConExTyCoVars dataCon
      sFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon (sTyArgs ++ existTys)
      tFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon (tTyArgs ++ existTys)
      fieldLabels = Ghc.flLabel <$> Ghc.dataConFieldLabels dataCon

  (sTy, tTy, label) <- zip3 sFieldTys tFieldTys fieldLabels

  if label == Ghc.FieldLabelString fieldName
     then [(sTy, aTy), (tTy, bTy)]
     else [(sTy, tTy)]
