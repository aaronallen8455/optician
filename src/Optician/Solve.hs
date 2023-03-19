module Optician.Solve
  ( solve
  ) where

import qualified GHC.TcPlugin.API as P

import           Optician.Inputs (Inputs(..))

solve :: Inputs -> P.TcPluginSolver
solve _ given wanted =
  pure $ P.TcPluginOk [] []
