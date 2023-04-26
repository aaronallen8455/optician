{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Optician.GhcFacade
  ( module Ghc
  , pattern ManyTy'
  , pattern FieldLabelString'
  , mkConstraintTupleTy'
  ) where

#if MIN_VERSION_ghc(9,6,0)
import           GHC.Core.TyCon as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Plugins as Ghc hiding (varName)
import           GHC.Tc.Types.Constraint as Ghc
import           Language.Haskell.Syntax.Basic as Ghc
import           GHC.Types.Name.Occurrence as Ghc
import           GHC.Tc.Utils.TcType as Ghc
import           GHC.Builtin.Names as Ghc
import           GHC.Tc.Types as Ghc hiding (DefaultingPlugin, TcPlugin)

#elif MIN_VERSION_ghc(9,4,0)
import           GHC.Core.TyCon as Ghc
import           GHC.Core.TyCo.Rep as Ghc
import           GHC.Plugins as Ghc hiding (extendTvSubst, isInScope, substTy, varName)
import           GHC.Tc.Types.Constraint as Ghc
import           GHC.Types.Name.Occurrence as Ghc
import           GHC.Builtin.Names as Ghc
import           GHC.Tc.Types as Ghc hiding (DefaultingPlugin, TcPlugin)
import           GHC.Tc.Utils.TcType as Ghc

#endif

pattern ManyTy' :: Ghc.Mult
#if MIN_VERSION_ghc(9,6,0)
pattern ManyTy' = Ghc.ManyTy
#elif MIN_VERSION_ghc(9,4,0)
pattern ManyTy' = Ghc.Many
#endif

pattern FieldLabelString' :: Ghc.FastString -> Ghc.FieldLabelString
#if MIN_VERSION_ghc(9,6,0)
pattern FieldLabelString' fs = Ghc.FieldLabelString fs
#elif MIN_VERSION_ghc(9,4,0)
pattern FieldLabelString' fs = fs
#endif

mkConstraintTupleTy' :: [Type] -> Type
#if MIN_VERSION_ghc(9,6,0)
mkConstraintTupleTy' = Ghc.mkConstraintTupleTy
#elif MIN_VERSION_ghc(9,4,0)
mkConstraintTupleTy' tys = Ghc.mkTyConApp (Ghc.cTupleTyCon (length tys)) tys
#endif
