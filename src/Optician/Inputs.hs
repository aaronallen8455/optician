{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Optician.Inputs
  ( Inputs(..)
  , lookupInputs
  ) where

import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc

data Inputs = MkInputs
  { getOpticKindTyCon :: P.TyCon
  , aLensType :: P.TcType
  , aPrismType :: P.TcType
  }

lookupInputs :: P.TcPluginM P.Init Inputs
lookupInputs = do
  typesResult <- P.findImportedModule (P.mkModuleName "Optician.Types")
                   =<< P.resolveImport (P.mkModuleName "Optician.Types") Nothing
  case typesResult of
    P.Found _ typesMod -> do
      getOpticKindName <- P.lookupOrig typesMod (P.mkTcOcc "GetOpticKind")
      getOpticKindTyCon <- P.lookupTyCon getOpticKindName
      aLensName <- P.lookupOrig typesMod (P.mkTcOcc "A_Lens")
      aLensType <- Ghc.varType <$> P.lookupId aLensName
      aPrismName <- P.lookupOrig typesMod (P.mkTcOcc "A_Prism")
      aPrismType <- Ghc.varType <$> P.lookupId aPrismName
      pure MkInputs{..}
    _ -> P.panic "Could not find 'Optician.Types'"
