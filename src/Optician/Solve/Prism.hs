{-# LANGUAGE DataKinds #-}

module Optician.Solve.Prism
  ( mkPrism
  ) where

import           Data.Foldable (find)
import           Data.Maybe (catMaybes)
import qualified GHC.TcPlugin.API as P
import qualified GHC.TcPlugin.API.Internal as PI

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs
import           Optician.Solve.Utils (mkWantedTypeEquality)

mkPrism
  :: Inputs
  -> Ghc.CtLoc
  -> [P.DataCon]
  -> [P.Type] -- type args for "s"
  -> [P.Type] -- type args for "t"
  -> P.FastString -- name of focused datacon
  -> P.Type -- s arg
  -> P.Type -- t arg
  -> P.Type -- a arg
  -> P.Type -- b arg
  -> P.TcPluginM P.Solve (Either [Ghc.Ct] P.CoreExpr)
mkPrism inp ctLoc dataCons sTyArgs tTyArgs focusedCon sArg tArg aArg bArg = do
  let matchFocusedCon = (== focusedCon) . Ghc.occNameFS . Ghc.nameOccName . Ghc.getName
  case find matchFocusedCon dataCons of
    Nothing -> pure (Left []) -- TODO emit custom error
    Just dataCon -> do
      sName <- PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "s")
      bName <- PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "b")
      wanteds <- mkPrismEqWanteds ctLoc dataCon sTyArgs tTyArgs aArg bArg
      if not (null wanteds)
         then pure (Left wanteds)
         else do
           let sBinder = Ghc.mkLocalIdOrCoVar sName Ghc.ManyTy sArg
               bBinder = Ghc.mkLocalIdOrCoVar bName Ghc.ManyTy bArg
               mInjectorExpr = mkInjectorExpr dataCon tTyArgs bBinder bArg
               getterExpr = mkGetterExpr inp dataCon sBinder tArg aArg
           pure $ do
             injectorExpr <- maybe (Left []) Right mInjectorExpr
             Right $ Ghc.mkCoreApps (Ghc.Var $ mkPrismId inp)
                       [ Ghc.Type sArg
                         , Ghc.Type tArg
                         , Ghc.Type aArg
                         , Ghc.Type bArg
                         , injectorExpr
                         , getterExpr
                         ]

-- | Make an expression of type b -> t
mkInjectorExpr
  :: P.DataCon
  -> [Ghc.Type] -- t ty args
  -> Ghc.Id -- binder for b
  -> Ghc.Type -- b
  -> Maybe Ghc.CoreExpr
mkInjectorExpr dataCon tTyArgs bBndr bTy
  | null (Ghc.dataConExTyCoVars dataCon) -- no existentials allowed
  , null $ Ghc.dataConTheta dataCon ++ Ghc.dataConStupidTheta dataCon -- no contexts
  = let mTupTys = extractTupleTys bTy
     in Just . Ghc.mkCoreLams [bBndr]
      $ case mTupTys of
          Nothing ->
            Ghc.mkCoreConApps dataCon
              $ (Ghc.Type <$> tTyArgs) ++ [Ghc.Var bBndr]
          Just (tupDataCon, tys) ->
            let bndrs = Ghc.mkTemplateLocalsNum 1 tys
             in Ghc.mkSingleAltCase (Ghc.Var bBndr) bBndr (Ghc.DataAlt tupDataCon) bndrs
                . Ghc.mkCoreConApps dataCon
                $ (Ghc.Type <$> tTyArgs)
               ++ (Ghc.Var <$> bndrs)

  | otherwise = Nothing

extractTupleTys :: Ghc.Type
                -> Maybe (Ghc.DataCon, [Ghc.Type]) -- returns Nothing if type is not a tuple
extractTupleTys = \case
  Ghc.TyConApp tyCon tyArgs
    | Ghc.isTupleTyCon tyCon
    , Just dataCon <- Ghc.tyConSingleDataCon_maybe tyCon
    -> Just (dataCon, tyArgs)
  _ -> Nothing

-- | Make an expression of type s -> Either t a
mkGetterExpr
  :: Inputs
  -> P.DataCon
  -> Ghc.Id -- binder for s
  -> Ghc.Type -- t
  -> Ghc.Type -- a
  -> Ghc.CoreExpr
mkGetterExpr inp dataCon scrutBndr tTy aTy =
  let resultTy = Ghc.mkTyConApp (eitherTyCon inp) [tTy, aTy]
      (tyVars, theta, _) = Ghc.tcSplitSigmaTy . Ghc.exprType . Ghc.Var
                         $ Ghc.dataConWorkId dataCon
      conCaseBndrs =
        Ghc.mkTemplateLocalsNum 1
          $ Ghc.scaledThing <$> Ghc.dataConRepArgTys dataCon
      valBndrs = drop (length tyVars + length theta) conCaseBndrs
      leftTExpr =
        Ghc.mkCoreConApps (leftDataCon inp)
          [Ghc.Type tTy, Ghc.Type aTy, Ghc.Var scrutBndr]
      rightAExpr =
        case valBndrs of
          [] -> Ghc.unitExpr
          bndrs ->
            Ghc.mkCoreConApps (rightDataCon inp)
              [Ghc.Type tTy, Ghc.Type aTy, Ghc.mkCoreTup $ Ghc.Var <$> bndrs]
      conAltCase = Ghc.Alt (Ghc.DataAlt dataCon) conCaseBndrs rightAExpr
   in Ghc.mkCoreLams [scrutBndr]
    $ Ghc.Case (Ghc.Var scrutBndr) scrutBndr resultTy
        [ Ghc.Alt Ghc.DEFAULT [] leftTExpr
        , conAltCase
        ]

mkPrismEqWanteds
  :: Ghc.CtLoc
  -> Ghc.DataCon
  -> [Ghc.Type] -- s ty args
  -> [Ghc.Type] -- t ty args
  -> Ghc.Type -- a
  -> Ghc.Type -- b
  -> P.TcPluginM P.Solve [Ghc.Ct]
mkPrismEqWanteds ctLoc dataCon sTyArgs tTyArgs aArg bArg = do
  let sFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon sTyArgs
      sTupleTy = Ghc.mkBoxedTupleTy sFieldTys
      tFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon tTyArgs
      tTupleTy = Ghc.mkBoxedTupleTy tFieldTys

  mAEq <-
    if Ghc.eqType aArg sTupleTy
       then pure Nothing
       else Just <$> mkWantedTypeEquality ctLoc aArg sTupleTy
  mBEq <-
    if Ghc.eqType bArg tTupleTy
       then pure Nothing
       else Just <$> mkWantedTypeEquality ctLoc bArg tTupleTy

  pure $ catMaybes [mAEq, mBEq]

-- TODO emit constraints for the non-focused data cons. They must match between
-- s and t for the getter to be sound
