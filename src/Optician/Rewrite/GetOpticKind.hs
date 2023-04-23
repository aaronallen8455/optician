module Optician.Rewrite.GetOpticKind
  ( getOpticKindRewriter
  , isLensLabel
  , lensDataCon
  , prismDataCons
  ) where

import           Control.Monad (guard)
import           Data.Char (isLowerCase)
import qualified Data.List as List
import           Data.Maybe (isJust)
import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs

getOpticKindRewriter :: Inputs -> P.TcPluginRewriter
getOpticKindRewriter inputs _givens [Ghc.LitTy (Ghc.StrTyLit labelArg), tyArg]
  | isLensLabel labelArg
  , isRecord tyArg = pure
      P.TcPluginRewriteTo
        { P.tcPluginReduction = mkReduction (aLensType inputs)
        , P.tcRewriterNewWanteds = []
        }
  | not (isLensLabel labelArg)
  , isSum tyArg = pure
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
getOpticKindRewriter _ _ _ = pure P.TcPluginNoRewrite

-- NB: this ignores the possibility of operator fields, oh well.
isLensLabel :: Ghc.FastString -> Bool
isLensLabel = all (\c -> isLowerCase c || c == '_') . take 1 . Ghc.unpackFS

lensDataCon :: Ghc.TyCon -> Ghc.FastString -> Maybe Ghc.DataCon
lensDataCon tyCon label = do
  guard $ isLensLabel label
  [dataCon] <- Ghc.tyConDataCons_maybe tyCon
  Just dataCon

prismDataCons :: Ghc.TyCon -> Ghc.FastString -> Maybe (Ghc.DataCon, [Ghc.DataCon])
prismDataCons tyCon label = do
  guard . not $ isLensLabel label
  dataCons <- Ghc.tyConDataCons_maybe tyCon
  let matchFocusedCon = (== label) . Ghc.occNameFS . Ghc.nameOccName . Ghc.getName
  ([dataCon], otherDataCons)
    <- Just $ List.partition matchFocusedCon dataCons
  pure (dataCon, otherDataCons)

isRecord :: P.Type -> Bool
isRecord = \case
  Ghc.TyConApp tyCon _ -> isJust $ Ghc.tyConSingleDataCon_maybe tyCon
  _ -> False

isSum :: P.Type -> Bool
isSum = \case
  Ghc.TyConApp tyCon _
    | Just dataCons <- Ghc.tyConDataCons_maybe tyCon
    -> not (null dataCons)
  _ -> False
