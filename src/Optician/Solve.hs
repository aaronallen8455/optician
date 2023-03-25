{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Optician.Solve
  ( solve
  ) where

import           Data.Maybe
import           Data.Traversable
import qualified GHC.TcPlugin.API as P
import qualified GHC.TcPlugin.API.Internal as PI

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs (Inputs(..))

solve :: Inputs -> P.TcPluginSolver
solve inputs _givens wanteds = do
  let wantedGenOptics = mapMaybe (isGenOptic inputs) wanteds
  solved <- fmap catMaybes $ for wantedGenOptics $ \(ct, args) ->
    (fmap . fmap) (\(evTerm, newWanteds) -> ((evTerm, ct), newWanteds))
                  (buildOptic inputs ct args)

  pure $ P.TcPluginOk (fst <$> solved) (concatMap snd solved)

isGenOptic :: Inputs -> P.Ct -> Maybe (P.Ct, [P.Type])
isGenOptic inputs = \case
  ct@Ghc.CDictCan{..}
    | cc_class == genOpticClass inputs
    -> Just (ct, cc_tyargs)
  _ ->  Nothing

buildOptic :: Inputs -> P.Ct -> [P.Type] -> P.TcPluginM P.Solve (Maybe (P.EvTerm, [P.Ct]))
buildOptic inputs ct [ Ghc.LitTy (Ghc.StrTyLit labelArg)
                      , sArg@(Ghc.TyConApp sTyCon sTyArgs)
                      , tArg@(Ghc.TyConApp _ tTyArgs)
                      , aArg
                      , bArg
                      ]
  | Just [dataCon] <- mDataCons -- product type
  = do
    mExpr <- mkLens inputs dataCon sTyArgs tTyArgs labelArg sArg tArg aArg bArg
    fieldEqWanteds <-
      mkFieldEqWanteds
        (Ghc.ctLoc ct)
        labelArg
        dataCon
        sTyArgs
        tTyArgs
        aArg
        bArg
    pure $ (\expr -> (P.EvExpr expr, fieldEqWanteds)) <$> mExpr
  | (not . null <$> mDataCons) == Just True
  = undefined -- sum type
  where
    mDataCons = Ghc.tyConDataCons_maybe sTyCon
buildOptic _ _ _ = pure Nothing

-- | Create wanted equality constraints for each field in the record so that
-- non-focused fields are the same between s and t and that a and b match
-- the focused field in their respective types.
mkFieldEqWanteds
  :: Ghc.CtLoc
  -> P.FastString
  -> P.DataCon
  -> [P.Type]
  -> [P.Type]
  -> P.Type
  -> P.Type
  -> P.TcPluginM P.Solve [Ghc.Ct]
mkFieldEqWanteds ctLoc fieldName dataCon sTyArgs tTyArgs aTy bTy = do
  let sFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon sTyArgs
      tFieldTys = Ghc.scaledThing <$> Ghc.dataConInstOrigArgTys dataCon tTyArgs
      fieldLabels = Ghc.flLabel <$> Ghc.dataConFieldLabels dataCon
  fmap concat . for (zip3 sFieldTys tFieldTys fieldLabels)
    $ \(sTy, tTy, label) ->
      if label == Ghc.FieldLabelString fieldName
         then do
           aEq <- mkWantedTypeEquality ctLoc sTy aTy
           bEq <- mkWantedTypeEquality ctLoc tTy bTy
           pure [aEq, bEq]
         else pure <$> mkWantedTypeEquality ctLoc sTy tTy

mkLens
  :: Inputs
  -> P.DataCon
  -> [P.Type] -- type args for "s"
  -> [P.Type] -- type args for "t"
  -> P.FastString
  -> P.Type -- s arg
  -> P.Type -- t arg
  -> P.Type -- a arg
  -> P.Type -- b arg
  -> P.TcPluginM P.Solve (Maybe P.CoreExpr)
mkLens inputs dataCon sTyArgs tTyArgs fieldName sArg tArg aArg bArg = do
  -- TODO check that dataCon is in scope
  fields <- zipWith (\i (a, b) -> (a, (i, b))) [0 :: Int ..]
              <$> saturatedSelectors dataCon sTyArgs
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
  -> [P.Type]
  -> P.TcPluginM P.Solve [(Ghc.FieldLabelString, Ghc.CoreExpr)]
saturatedSelectors dataCon tyArgs = do
  let tcSubst = Ghc.zipTCvSubst (Ghc.dataConUserTyVars dataCon) tyArgs
  for (Ghc.dataConFieldLabels dataCon) $ \fieldLabel -> do
    selId <- P.tcLookupId $ Ghc.flSelector fieldLabel
    pure (Ghc.flLabel fieldLabel, instantiateSelector (Ghc.Var selId) tcSubst)

instantiateSelector :: Ghc.CoreExpr -> Ghc.Subst -> Ghc.CoreExpr
instantiateSelector sel tcSubst = do
  case Ghc.splitForAllTyCoVar_maybe (Ghc.exprType sel) of
    Just (arg, _)
      | Ghc.isTyVar arg ->
          let applied = case Ghc.lookupTyVar tcSubst arg of
                Nothing -> Ghc.mkCoreApps sel [Ghc.Var arg]
                Just sub -> Ghc.mkCoreApps sel [Ghc.Type sub]
           in instantiateSelector applied tcSubst
    _ -> sel

mkWantedTypeEquality :: Ghc.CtLoc -> Ghc.Type -> Ghc.Type -> P.TcPluginM P.Solve Ghc.Ct
mkWantedTypeEquality ctLoc sTy tTy =
  fmap Ghc.mkNonCanonical
    . P.newWanted ctLoc
    $ P.mkPrimEqPredRole Ghc.Representational sTy tTy

-- Polymorphic updates will be tricky because need some way to know that all
-- occurrences of a given ty var will be changed by the operation. Need some
-- way to look through all the other fields and make sure that the ty var
-- doesn't occur anywhere in their types.
-- There could also be multiple ty vars impacted by a single optic, so will
-- need to tease out the individual vars and do this check for each of them.
-- Equality constraints will need to be emitted for the t, a, and b args based
-- on the actual types.
--
-- For products, take the type of the field and compare it to the a arg
-- (will need to emit equality for a and the instantiated field of s)
-- this should allow for determining which of the polymorphic variables occur
-- in the field's type and their position.
-- Then examine the b argument and see if it changes the type of any of the
-- variable positions. For those that do change, check that the variable does
-- not occur in other fields.
-- (emit equality for b and the field type of t)
--
--
-- A different perhaps easier way to go:
-- Compare the s and t types to see which ty args change, if any. For those that
-- change, check that they only occur in the focused field. emit constraints
-- tying a and b to the field in s and t respectively.
--
--
-- An even easier though seemingly less efficient way:
-- Emit equality constraints between all fields in s vs al fields in t except
-- for the focused field, emit equalit against the expected field types for
-- both of those. This seems like it would yield the best error messages.
--
-- Will probably need some sort of guard against existentially quantified ty vars.
