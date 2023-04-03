module Optician
  ( optic
  , field
  , _Ctor
  , GenOptic
  , GetOpticKind
  , plugin
  ) where

import qualified GHC.TcPlugin.API as P
import           Optician.Types (GenOptic, GetOpticKind, optic, field, _Ctor)

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs (lookupInputs)
import           Optician.Rewrite (rewrite)
import           Optician.Solve (solve)

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.tcPlugin = const $ Just (P.mkTcPlugin tcPlugin)
  , Ghc.pluginRecompile = Ghc.purePlugin
  }

tcPlugin :: P.TcPlugin
tcPlugin = P.TcPlugin
  { P.tcPluginInit = lookupInputs
  , P.tcPluginSolve = solve
  , P.tcPluginRewrite = rewrite
  , P.tcPluginStop = const $ pure ()
  }
