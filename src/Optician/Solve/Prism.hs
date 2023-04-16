{-# LANGUAGE DataKinds #-}

module Optician.Solve.Prism
  ( mkPrism
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           Data.Maybe
import qualified GHC.TcPlugin.API as P
import qualified GHC.TcPlugin.API.Internal as PI

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs
import qualified Optician.TypeErrors as Err

mkPrism
  :: Inputs
  -> Ghc.CtLoc
  -> P.DataCon -- focused datacon
  -> [P.Type] -- type args for "t"
  -> P.Type -- s arg
  -> P.Type -- t arg
  -> P.Type -- a arg
  -> P.Type -- b arg
  -> P.TcPluginM P.Solve (Either Err.OpticErr (P.CoreExpr, [Ghc.Ct]))
mkPrism inp ctLoc dataCon tTyArgs sArg tArg aArg bArg = runExceptT $ do
  sName <- lift . PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "s")
  bName <- lift . PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "b")
  let sBinder = Ghc.mkLocalIdOrCoVar sName Ghc.ManyTy sArg
      bBinder = Ghc.mkLocalIdOrCoVar bName Ghc.ManyTy bArg
      getterExpr = mkGetterExpr inp dataCon sBinder tArg aArg
  (injectorExpr, wanteds) <- mkInjectorExpr ctLoc dataCon tTyArgs bBinder bArg
  pure ( Ghc.mkCoreApps (Ghc.Var $ mkPrismId inp)
           [ Ghc.Type sArg
           , Ghc.Type tArg
           , Ghc.Type aArg
           , Ghc.Type bArg
           , injectorExpr
           , getterExpr
           ]
       , wanteds
       )

-- | Make an expression of type b -> t
mkInjectorExpr
  :: Ghc.CtLoc
  -> P.DataCon
  -> [Ghc.Type] -- t ty args
  -> Ghc.Id -- binder for b
  -> Ghc.Type -- b
  -> ExceptT Err.OpticErr (P.TcPluginM P.Solve) (Ghc.CoreExpr, [Ghc.Ct])
mkInjectorExpr ctLoc dataCon tTyArgs bBndr bTy = do
  unless (null $ Ghc.dataConStupidTheta dataCon)
    $ throwE Err.StupidTheta

  gre <- lift $ Ghc.tcg_rdr_env . fst <$> P.getEnvs
  when (isNothing . Ghc.lookupGRE_Name gre $ Ghc.getName dataCon)
    $ throwE (Err.DataConNotInScope dataCon)

  unless (null (Ghc.dataConExTyCoVars dataCon)) -- no existentials allowed
    $ throwE Err.PrismDataConHasExistentials

  let mTupTys = extractTupleTys bTy
      universalTys = Ghc.dataConUnivTyVars dataCon
      (_, theta, _) = Ghc.tcSplitSigmaTy . Ghc.idType
                    $ Ghc.dataConWorkId dataCon
      tSubst = Ghc.zipTvSubst universalTys tTyArgs
      tTheta = Ghc.substTy tSubst <$> theta

  newWanteds <- lift $ traverse (P.newWanted ctLoc) tTheta

  let thetaBndrs = Ghc.ctEvEvId <$> newWanteds
      expr = Ghc.mkCoreLams [bBndr] $
        case mTupTys of
          -- Only universal ty args are supplied because it was already
          -- asserted that there are no existentials.
          Nothing ->
            Ghc.mkCoreConApps dataCon
              $ (Ghc.Type <$> tTyArgs)
             ++ (Ghc.Var <$> thetaBndrs)
             ++ [Ghc.Var bBndr]
          Just (tupDataCon, tys) ->
            let bndrs = Ghc.mkTemplateLocalsNum 1 tys
             in Ghc.mkSingleAltCase (Ghc.Var bBndr) bBndr (Ghc.DataAlt tupDataCon) bndrs
                . Ghc.mkCoreConApps dataCon
                $ (Ghc.Type <$> tTyArgs)
               ++ (Ghc.Var <$> (thetaBndrs ++ bndrs))

  pure (expr, Ghc.mkNonCanonical <$> newWanteds)

-- | The 'b' type will either be a single type or a tuple of all the args to
-- the data con.
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
      (_, theta, _) = Ghc.tcSplitSigmaTy . Ghc.idType
                    $ Ghc.dataConWorkId dataCon
      conCaseBndrs =
        Ghc.mkTemplateLocalsNum 1
          $ Ghc.scaledThing <$> Ghc.dataConRepArgTys dataCon
      -- already asserted that there are no existentials or thetas
      valBndrs = drop (length theta) conCaseBndrs
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
