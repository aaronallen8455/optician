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
  , genOpticClass :: P.Class
  , mkLensId :: P.Id
  , mkPrismId :: P.Id
  , eitherTyCon :: P.TyCon
  , leftDataCon :: P.DataCon
  , rightDataCon :: P.DataCon
  , genTypeEqualitiesTyCon :: P.TyCon
  }

lookupInputs :: P.TcPluginM P.Init Inputs
lookupInputs = do
  typesResult <- P.findImportedModule (P.mkModuleName "Optician.Internal")
                   =<< P.resolveImport (P.mkModuleName "Optician.Internal") Nothing
  case typesResult of
    P.Found _ typesMod -> do
      getOpticKindName <- P.lookupOrig typesMod (P.mkTcOcc "GetOpticKind")
      getOpticKindTyCon <- P.lookupTyCon getOpticKindName
      aLensName <- P.lookupOrig typesMod (P.mkTcOcc "ALens")
      mALensType <- Ghc.synTyConRhs_maybe <$> P.lookupTyCon aLensName
      aPrismName <- P.lookupOrig typesMod (P.mkTcOcc "APrism")
      mAPrismType <- Ghc.synTyConRhs_maybe <$> P.lookupTyCon aPrismName
      genOpticClass <- P.tcLookupClass =<< P.lookupOrig typesMod (P.mkTcOcc "GenOptic")
      mkLensId <- P.tcLookupId =<< P.lookupOrig typesMod (P.mkVarOcc "mkLens")
      mkPrismId <- P.tcLookupId =<< P.lookupOrig typesMod (P.mkVarOcc "mkPrism")
      eitherTyCon <- P.lookupTyCon Ghc.eitherTyConName
      leftDataCon <- P.tcLookupDataCon Ghc.leftDataConName
      rightDataCon <- P.tcLookupDataCon Ghc.rightDataConName
      genTypeEqualitiesName <- P.lookupOrig typesMod (P.mkTcOcc "GenTypeEqualities")
      genTypeEqualitiesTyCon <- P.lookupTyCon genTypeEqualitiesName
      case (,) <$> mALensType <*> mAPrismType of
        Nothing -> P.panic "Could not get optic types"
        Just (aLensType, aPrismType) -> pure MkInputs{..}
    _ -> P.panic "Could not find 'Optician.Internal"
