{-# LANGUAGE DataKinds #-}

module Optician.Solve.Prism
  ( mkPrism
  ) where

import qualified GHC.TcPlugin.API as P
import qualified GHC.TcPlugin.API.Internal as PI

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs

mkPrism
  :: Inputs
  -> P.DataCon -- focused datacon
  -> [P.Type] -- type args for "t"
  -> P.Type -- s arg
  -> P.Type -- t arg
  -> P.Type -- a arg
  -> P.Type -- b arg
  -> P.TcPluginM P.Solve (Maybe P.CoreExpr)
mkPrism inp dataCon tTyArgs sArg tArg aArg bArg = do
  sName <- PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "s")
  bName <- PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "b")
  let sBinder = Ghc.mkLocalIdOrCoVar sName Ghc.ManyTy sArg
      bBinder = Ghc.mkLocalIdOrCoVar bName Ghc.ManyTy bArg
      mInjectorExpr = mkInjectorExpr dataCon tTyArgs bBinder bArg
      getterExpr = mkGetterExpr inp dataCon sBinder tArg aArg
  pure $ do
    injectorExpr <- mInjectorExpr
    Just $ Ghc.mkCoreApps (Ghc.Var $ mkPrismId inp)
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

-- | Make an expression of type s -> Either t a. Assumes that the s arg can
-- be used for t, type checking is done elsewhere.
mkGetterExpr
  :: Inputs
  -> P.DataCon
  -> Ghc.Id -- binder for s
  -> Ghc.Type -- t
  -> Ghc.Type -- a
  -> Ghc.CoreExpr
mkGetterExpr inp dataCon scrutBndr tTy aTy =
  let resultTy = Ghc.mkTyConApp (eitherTyCon inp) [tTy, aTy]
--       (_, theta, _) = Ghc.tcSplitSigmaTy . Ghc.exprType . Ghc.Var
--                          $ Ghc.dataConWorkId dataCon
      conCaseBndrs =
        Ghc.mkTemplateLocalsNum 1
          $ Ghc.scaledThing <$> Ghc.dataConRepArgTys dataCon
      -- already asserted that there are no existentials or thetas
      valBndrs = conCaseBndrs -- drop (length theta) conCaseBndrs
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

-- GHC simply piles up the irreducible constraints that get emitted and keeps
-- trying to re-solve the GenOptic instance which emits those same constraints.
-- Could fish out the CIrredCans from the wanteds and check if they have the
-- same type as the cts generated here and not reemit if so. Or perhaps just
-- any of the wanteds, doesn't have to be irred.
-- Could also have a type family that rewrites to a constraint tuple containing
-- all the relevant equalities. Then the solver portion simply fails of the
-- types do not align perfectly. This is how the SameBase family is used.
