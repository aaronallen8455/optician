module Optician.Rewrite
  ( rewrite
  ) where

import           Data.Maybe
import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs (Inputs(..))

rewrite :: Inputs -> P.UniqFM P.TyCon P.TcPluginRewriter
rewrite inputs = P.listToUFM [(getOpticKindTyCon inputs, rewriter inputs)]

rewriter :: Inputs -> P.TcPluginRewriter
rewriter inputs _givens [tyArg]
  | isRecord tyArg = pure
      P.TcPluginRewriteTo
        { P.tcPluginReduction = mkReduction (aLensType inputs)
        , P.tcRewriterNewWanteds = []
        }
  | isSum tyArg = pure
      P.TcPluginRewriteTo
        { P.tcPluginReduction = mkReduction (aPrismType inputs)
        , P.tcRewriterNewWanteds = []
        }
  | otherwise = pure P.TcPluginNoRewrite
  where
    mkReduction =
      P.mkTyFamAppReduction
        "GetOpticKind"
        P.Nominal
        (getOpticKindTyCon inputs)
        [tyArg]
rewriter _ _ _ = pure P.TcPluginNoRewrite

isRecord :: P.Type -> Bool
isRecord = \case
  Ghc.TyConApp tyCon _ -> isJust $ Ghc.tyConSingleDataCon_maybe tyCon
  _ -> False

isSum :: P.Type -> Bool
isSum = \case
  Ghc.TyConApp tyCon _
    | Just dataCons <- Ghc.tyConDataCons_maybe tyCon
    -> length dataCons > 1
  _ -> False
