module Optician.TypeErrors
  ( OpticErr(..)
  , opticErrorToCt
  ) where

import           Control.Monad
import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc

data OpticErr
  = MissingField !Ghc.Type !Ghc.FastString
  | DataConNotInScope !Ghc.DataCon
  | SelectorHasExistential

opticErrorToCt :: P.MonadTcPluginWork m => Ghc.CtLoc -> OpticErr -> m Ghc.Ct
opticErrorToCt ctLoc err
  = fmap Ghc.mkNonCanonical . P.newWanted ctLoc <=< P.mkTcPluginErrorTy
  $ case err of
      MissingField ty fieldName ->
        P.PrintType ty P.:|: P.Txt " does not have field "
          P.:|: P.PrintType (Ghc.mkStrLitTy fieldName)
      DataConNotInScope dataCon ->
        P.Txt "Data constructor not in scope: "
          P.:|: P.PrintType (Ghc.mkStrLitTy . Ghc.occNameFS . Ghc.occName $ Ghc.getName dataCon)
      SelectorHasExistential ->
        P.Txt "Cannot make an optic for a field with an existential type"
