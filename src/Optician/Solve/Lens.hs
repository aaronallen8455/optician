{-# LANGUAGE DataKinds #-}
module Optician.Solve.Lens
  ( mkLens
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           Data.Maybe
import           Data.Traversable (for)
import qualified GHC.TcPlugin.API as P
import qualified GHC.TcPlugin.API.Internal as PI

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs
import qualified Optician.TypeErrors as Err

-- | Builds the core expr for a lens. This relies on the necessary type equality
-- checks occurring elsewhere, otherwise unsound expressions can result.
mkLens
  :: Inputs
  -> Ghc.CtLoc
  -> P.DataCon
  -> P.FastString -- label of focused field
  -> [P.Type] -- s ty args
  -> [P.Type] -- t ty args
  -> P.Type -- s arg
  -> P.Type -- t arg
  -> P.Type -- a arg
  -> P.Type -- b arg
  -> P.TcPluginM P.Solve (Either Err.OpticErr (P.CoreExpr, [Ghc.Ct])) -- Cts are new wanteds based on theta context of data con
mkLens inputs ctLoc dataCon fieldName sTyArgs tTyArgs sArg tArg aArg bArg = runExceptT $ do
  unless (null $ Ghc.dataConStupidTheta dataCon)
    $ throwE Err.StupidTheta

  gre <- lift $ Ghc.tcg_rdr_env . fst <$> P.getEnvs
  when (isNothing . Ghc.lookupGRE_Name gre $ Ghc.getName dataCon)
    $ throwE (Err.DataConNotInScope dataCon)

  fields <- zipWith (\i (a, b, c) -> (a, (i, b, c))) [0 :: Int ..]
              <$> lift (saturatedSelectors dataCon)

  when (null fields)
    $ throwE (Err.DataConWithoutFields dataCon)

  (focusIndex, focusSel, selId) <-
    case lookup (Ghc.FieldLabelString fieldName) fields of
      Nothing -> throwE $ Err.MissingField sArg fieldName
      Just x -> pure x

  when (Ghc.isNaughtyRecordSelector selId) $
    throwE Err.SelectorHasExistential

  sName <- lift . PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "s")
  let sBinder = Ghc.mkLocalIdOrCoVar sName Ghc.ManyTy sArg

  bName <- lift . PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "b")
  let bBinder = Ghc.mkLocalIdOrCoVar bName Ghc.ManyTy bArg
      (_, theta, _) = Ghc.tcSplitSigmaTy . Ghc.idType
                    $ Ghc.dataConWorkId dataCon

  -- Need to instantiate the types of theta and check equality between s and t.
  -- Only emit wanteds for those that differ, otherwise pass the dict through.
  let universalTys = Ghc.dataConUnivTyVars dataCon
      existTys = Ghc.dataConExTyCoVars dataCon
      sSubst = Ghc.zipTvSubst universalTys sTyArgs
      tSubst = Ghc.zipTvSubst universalTys tTyArgs
      sTheta = Ghc.substTy sSubst <$> theta
      tTheta = Ghc.substTy tSubst <$> theta
      diffTheta = zipWith f sTheta tTheta
        where
          f a b | Ghc.eqType a b = Nothing
                | otherwise = Just b

  newWanteds <- lift $ (traverse . traverse) (P.newWanted ctLoc) diffTheta

  let workerArgsTys = Ghc.dataConRepArgTys dataCon
      tyArgs = universalTys ++ existTys
      bndrs = Ghc.mkTemplateLocalsNum 1 (Ghc.scaledThing <$> workerArgsTys)
      thetaBndrs = do -- List
        (mWanted, bndr) <- zip newWanteds bndrs
        [maybe bndr Ghc.ctEvEvId mWanted]
      resultDataConArgs = thetaBndrs ++ do -- List
        (ix, bndr) <- zip [0..] $ drop (length theta) bndrs

        if ix == focusIndex
           then [bBinder]
           else [bndr]
      setterExpr = Ghc.mkCoreLams [sBinder, bBinder] $
        Ghc.mkSingleAltCase
          (Ghc.Var sBinder)
          sBinder
          (Ghc.DataAlt dataCon)
          (existTys ++ bndrs)
          setterBody
      -- NB: only the existential ty vars are included in the case binders but
      -- universal *and* existential are used as args to the data con.
      setterBody = Ghc.mkCoreConApps dataCon
        $ (Ghc.Type . Ghc.mkTyVarTy <$> tyArgs) ++ (Ghc.Var <$> resultDataConArgs)

  pure
    ( Ghc.mkCoreApps
        (Ghc.Var $ mkLensId inputs)
        [ Ghc.Type sArg
        , Ghc.Type tArg
        , Ghc.Type aArg
        , Ghc.Type bArg
        , focusSel
        , setterExpr
        ]
    , Ghc.mkNonCanonical <$> catMaybes newWanteds
    )

-- | Returns field selectors with type arguments applied
saturatedSelectors
  :: P.DataCon
  -> P.TcPluginM P.Solve [(Ghc.FieldLabelString, Ghc.CoreExpr, Ghc.Id)]
saturatedSelectors dataCon = do
  for (Ghc.dataConFieldLabels dataCon) $ \fieldLabel -> do
    selId <- P.tcLookupId $ Ghc.flSelector fieldLabel
    pure (Ghc.flLabel fieldLabel, instantiateSelector (Ghc.Var selId), selId)

instantiateSelector :: Ghc.CoreExpr -> Ghc.CoreExpr
instantiateSelector sel =
  case Ghc.splitForAllTyCoVar_maybe $ Ghc.exprType sel of
    Nothing -> sel
    -- It doesn't seem to matter what ty vars are applied to the selector, hence Any.
    Just (arg, _) ->
      instantiateSelector $
        Ghc.mkCoreApps sel [Ghc.Type . Ghc.anyTypeOfKind $ Ghc.varType arg]
