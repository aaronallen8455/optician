module Optician.Rewrite.GenTypeEqualities
  ( genTypeEqualitiesRewriter
  ) where

import qualified Data.List as List
import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs
import           Optician.Rewrite.GenTypeEqualities.Lens (lensTyEqPairs)
import           Optician.Rewrite.GenTypeEqualities.Prism (prismTyEqPairs)

genTypeEqualitiesRewriter :: Inputs -> P.TcPluginRewriter
genTypeEqualitiesRewriter inputs _givens
        tyArgs@[ Ghc.LitTy (Ghc.StrTyLit labelArg)
               , Ghc.TyConApp sTyCon sTyArgs
               , Ghc.TyConApp _tTyCon tTyArgs
               , aArg
               , bArg
               ]
  -- product type
  | Just [dataCon] <- mDataCons
  = pure P.TcPluginRewriteTo
    { P.tcPluginReduction
        = mkReduction
        . mkConstraintFromPairs
        $ lensTyEqPairs labelArg sTyCon dataCon sTyArgs tTyArgs aArg bArg
    , P.tcRewriterNewWanteds = []
    }

  -- sum type
  | Just dataCons <- mDataCons
  , let matchFocusedCon = (== labelArg) . Ghc.occNameFS . Ghc.nameOccName . Ghc.getName
  , ([dataCon], otherDataCons) <- List.partition matchFocusedCon dataCons
  = pure P.TcPluginRewriteTo
      { P.tcPluginReduction
          = mkReduction
          . mkConstraintFromPairs
          $ prismTyEqPairs sTyCon dataCon otherDataCons sTyArgs tTyArgs aArg bArg
      , P.tcRewriterNewWanteds = []
      }

  | otherwise
  = pure P.TcPluginRewriteTo
      { P.tcPluginReduction = mkReduction $ Ghc.mkConstraintTupleTy [] -- empty constraint
      , P.tcRewriterNewWanteds = []
      }
  where
    mDataCons = Ghc.tyConDataCons_maybe sTyCon
    mkReduction =
      P.mkTyFamAppReduction
        "GenTypeEqualities"
        P.Nominal
        (genTypeEqualitiesTyCon inputs)
        tyArgs
genTypeEqualitiesRewriter _ _ _ = pure P.TcPluginNoRewrite

mkConstraintFromPairs :: [(P.Type, P.Type)] -> P.Type
mkConstraintFromPairs pairs =
  Ghc.mkConstraintTupleTy
    $ uncurry Ghc.mkPrimEqPred <$> pairs
