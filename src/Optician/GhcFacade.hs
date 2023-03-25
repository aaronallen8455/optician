module Optician.GhcFacade
  ( module Ghc
  ) where

import           GHC.Core.TyCon as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Plugins as Ghc hiding (varName)
import           GHC.Tc.Types.Constraint as Ghc
import           Language.Haskell.Syntax.Basic as Ghc
import           GHC.Types.Name.Occurrence as Ghc