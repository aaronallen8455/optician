{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Optician.Solve
  ( solve
  ) where

import           Data.Bifunctor (first)
import           Data.Maybe
import           Data.Traversable
import qualified GHC.TcPlugin.API as P

import qualified Optician.GhcFacade as Ghc
import           Optician.Inputs (Inputs(..))
import           Optician.Solve.Lens (mkLens, mkFieldEqWanteds)
import           Optician.Solve.Prism (mkPrism)

solve :: Inputs -> P.TcPluginSolver
solve inputs _givens wanteds = do
  let wantedGenOptics = mapMaybe (isGenOptic inputs) wanteds
  solved <- for wantedGenOptics $ \(ct, args) ->
    fmap (first (fmap (, ct)))
         (buildOptic inputs ct args)

  pure $ P.TcPluginOk (mapMaybe fst solved) (concatMap snd solved)

-- | Identify wanteds for the GenOptic class
isGenOptic :: Inputs -> P.Ct -> Maybe (P.Ct, [P.Type])
isGenOptic inputs = \case
  ct@Ghc.CDictCan{..}
    | cc_class == genOpticClass inputs
    -> Just (ct, cc_tyargs)
  _ ->  Nothing

-- seems possible in theory to support existentials using mkSingleAltCase and
-- dataConrepArgTys, but need to somehow pick the binder that corresponds to
-- the focused field. Skip over all the things that have kind Type or Constraint?
--
-- How it could work:
-- split the sigma ty of the datacon worker ID to get the free vars and predicates
-- separated out from the value args tys.
-- Build a map from the user ty args to the tycon ty args.
-- Use that map to instantiate the pred types. Is instantiation really necessary?
-- Check if there are any preds that are different between the s and t types.
-- Create wanted constraints for those preds if so.
-- Determine the index position of the focused field in the value arg types.
-- Check if the focused field has a naughty selector - fail out if so b/c its an existential.
-- To build the setter arg to 'lens', use mkSingleAltCase with the datacon and the
-- binder for the s arg. Use the arg types for the constructor worker 'dataConRepArgTys'
-- to make the binders for the case pattern (see mkDictSelRhs for an example).
-- Now use mkCoreConApps with the datacon giving it the universal ty args from
-- the t tyCon, the existential tys using the binders, binders for preds that
-- did not change, the evidence vars for preds that are still wanted (does this work?),
-- binders for field values other than the focused field, the b arg binder for the
-- focused field.
-- Should reject stupid thetas

-- | Attemps to build an optic to satisfy a GenClass wanted
buildOptic :: Inputs -> P.Ct -> [P.Type] -> P.TcPluginM P.Solve (Maybe P.EvTerm, [P.Ct])
buildOptic inputs ct [ Ghc.LitTy (Ghc.StrTyLit labelArg)
                      , sArg@(Ghc.TyConApp sTyCon sTyArgs)
                      , tArg@(Ghc.TyConApp tTyCon tTyArgs)
                      , aArg
                      , bArg
                      ]
  -- product type
  | Just [dataCon] <- mDataCons
  , sTyCon == tTyCon -- fail so that the SameBase constraint will attempt to solve this equality
  -- TODO allow existentials and contexts
  , null (Ghc.dataConExTyCoVars dataCon) -- no existentials allowed
  , null $ Ghc.dataConTheta dataCon ++ Ghc.dataConStupidTheta dataCon -- no contexts
  = do
    mExpr <- mkLens inputs dataCon tTyArgs labelArg sArg tArg aArg bArg
    fieldEqWanteds <-
      mkFieldEqWanteds
        (Ghc.ctLoc ct)
        labelArg
        dataCon
        sTyArgs
        tTyArgs
        aArg
        bArg
    pure (P.EvExpr <$> mExpr, fieldEqWanteds)

  -- sum type
  | Just dataCons <- mDataCons
  , sTyCon == tTyCon -- fail so that the SameBase constraint will attempt to solve this equality
  -- don't allow any data cons to have existentials or contexts
  , not (null dataCons)
  -- TODO should be possible to allow them
  , all (null . Ghc.dataConExTyCoVars) dataCons -- no existentials allowed
  , all (null . Ghc.dataConTheta) dataCons -- no contexts
  , all (null . Ghc.dataConStupidTheta) dataCons
  = do
    eExpr <- mkPrism inputs (Ghc.ctLoc ct) dataCons sTyArgs tTyArgs labelArg sArg tArg aArg bArg
    pure $ case eExpr of
      Left ws -> (Nothing, ws)
      Right expr -> (Just $ P.EvExpr expr, [])

  where
    mDataCons = Ghc.tyConDataCons_maybe sTyCon
buildOptic _ _ _ = pure (Nothing, [])

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
