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
  | DataConWithoutFields !Ghc.DataCon
  | TypeDoesNotHaveDataCon !Ghc.Type !Ghc.FastString
  | PrismDataConHasExistentials
  | StupidTheta

opticErrorToCt :: P.MonadTcPluginWork m => Ghc.CtLoc -> OpticErr -> m Ghc.Ct
opticErrorToCt ctLoc err
  = fmap Ghc.mkNonCanonical . P.newWanted ctLoc <=< P.mkTcPluginErrorTy
  $ case err of
      MissingField ty fieldName ->
        P.PrintType ty P.:|: P.Txt " does not have field "
          P.:|: P.PrintType (Ghc.mkStrLitTy fieldName)
      DataConNotInScope dataCon ->
        P.Txt "Data constructor not in scope: "
          P.:|: P.PrintType (showDataCon dataCon)
      SelectorHasExistential ->
        P.Txt "Cannot make a lens for a field with an existential type"
      DataConWithoutFields dataCon ->
        P.Txt "Cannot make a lens for a data constructor that does not have fields: "
          P.:|: P.PrintType (showDataCon dataCon)
      TypeDoesNotHaveDataCon ty label ->
        P.Txt "Type '" P.:|: P.PrintType ty
          P.:|: P.Txt "' does not have data constructor: "
          P.:|: P.PrintType (Ghc.mkStrLitTy label)
      PrismDataConHasExistentials ->
        P.Txt "Cannot make a prism for a data constructor with an existential type"
      StupidTheta ->
        P.Txt "Cannot make an optics for data constructors with a 'stupid theta'"

showDataCon :: Ghc.DataCon -> Ghc.Type
showDataCon = Ghc.mkStrLitTy . Ghc.occNameFS . Ghc.occName . Ghc.getName
