module Optician.Rewrite
  ( rewrite
  ) where

import qualified GHC.TcPlugin.API as P

import           Optician.Inputs (Inputs(..))
import           Optician.Rewrite.GetOpticKind (getOpticKindRewriter)
import           Optician.Rewrite.GenTypeEqualities (genTypeEqualitiesRewriter)

rewrite :: Inputs -> P.UniqFM P.TyCon P.TcPluginRewriter
rewrite inputs =
  P.listToUFM
    [ (getOpticKindTyCon inputs, getOpticKindRewriter inputs)
    , (genTypeEqualitiesTyCon inputs, genTypeEqualitiesRewriter inputs)
    ]
