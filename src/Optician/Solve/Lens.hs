{-# LANGUAGE DataKinds #-}
module Optician.Solve.Lens
  ( mkLens
  ) where

import           Data.Traversable (for)
import qualified GHC.TcPlugin.API as P
import qualified GHC.TcPlugin.API.Internal as PI

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs

-- | Builds the core expr for a lens. This relies on the necessary type equality
-- checks occurring elsewhere, otherwise unsound expressions can result.
mkLens
  :: Inputs
  -> P.DataCon
  -> [P.Type] -- type args for "t"
  -> P.FastString -- label of focused field
  -> P.Type -- s arg
  -> P.Type -- t arg
  -> P.Type -- a arg
  -> P.Type -- b arg
  -> P.TcPluginM P.Solve (Maybe P.CoreExpr)
mkLens inputs dataCon tTyArgs fieldName sArg tArg aArg bArg = do
  -- TODO check that dataCon is in scope
  fields <- zipWith (\i (a, b) -> (a, (i, b))) [0 :: Int ..]
              <$> saturatedSelectors dataCon
  for (lookup (Ghc.FieldLabelString fieldName) fields)
      $ \(focusIndex, focusSel) -> do
    sName <- PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "s")
    let sBinder = Ghc.mkLocalIdOrCoVar sName Ghc.ManyTy sArg
    bName <- PI.unsafeLiftTcM $ P.newName (Ghc.mkOccName Ghc.varName "b")
    let bBinder = Ghc.mkLocalIdOrCoVar bName Ghc.ManyTy bArg
        mkFieldSetterExpr (_, (i, selector)) =
          if i == focusIndex
             then Ghc.Var bBinder
             else Ghc.mkCoreApps selector [Ghc.Var sBinder]
        setterExpr = Ghc.mkCoreLams [sBinder, bBinder] $
          Ghc.mkCoreConApps dataCon
            $ (Ghc.Type <$> tTyArgs)
           ++ (mkFieldSetterExpr <$> fields)

    pure $
      Ghc.mkCoreApps
        (Ghc.Var $ mkLensId inputs)
        [ Ghc.Type sArg
        , Ghc.Type tArg
        , Ghc.Type aArg
        , Ghc.Type bArg
        , focusSel
        , setterExpr
        ]

-- | Returns field selectors with type arguments applied
saturatedSelectors
  :: P.DataCon
  -> P.TcPluginM P.Solve [(Ghc.FieldLabelString, Ghc.CoreExpr)]
saturatedSelectors dataCon = do
--   let tcSubst = Ghc.zipTCvSubst (Ghc.dataConUserTyVars dataCon) tyArgs
--   traceM $ Ghc.showSDocUnsafe $ Ghc.ppr tcSubst
  for (Ghc.dataConFieldLabels dataCon) $ \fieldLabel -> do
    selId <- P.tcLookupId $ Ghc.flSelector fieldLabel
    pure (Ghc.flLabel fieldLabel, instantiateSelector (Ghc.Var selId)) --tyArgs)

instantiateSelector :: Ghc.CoreExpr -> Ghc.CoreExpr
instantiateSelector sel =
  case Ghc.splitForAllTyCoVar_maybe $ Ghc.exprType sel of
    Nothing -> sel
    -- It doesn't seem to matter what ty vars are applied to the selector, hence Any.
    Just (arg, _) ->
      instantiateSelector $
        Ghc.mkCoreApps sel [Ghc.Type . Ghc.anyTypeOfKind $ Ghc.varType arg]
