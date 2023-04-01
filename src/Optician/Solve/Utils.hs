{-# LANGUAGE DataKinds #-}
module Optician.Solve.Utils
  ( mkWantedTypeEquality
  ) where

import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc

-- | Generates a wanted constraint for nominal equality of two types
mkWantedTypeEquality :: Ghc.CtLoc -> Ghc.Type -> Ghc.Type -> P.TcPluginM P.Solve Ghc.Ct
mkWantedTypeEquality ctLoc sTy tTy =
  fmap Ghc.mkNonCanonical
    . P.newWanted ctLoc
    $ P.mkPrimEqPredRole Ghc.Nominal sTy tTy
