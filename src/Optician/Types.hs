{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
module Optician.Types
  ( GenOptic(..)
  , optic
  , field
  , _Ctor
  , GetOpticKind
  , ALens
  , APrism
  , mkLens
  , mkPrism
  ) where

import           Data.Kind
import           GHC.TypeLits
import qualified Optics.Lens as L
import           Optics.Optic
import qualified Optics.Prism as P

type ALens = L.A_Lens
type APrism = P.A_Prism

optic :: forall (label :: Symbol) s t a b
       . ( SameBase s t
         , GenOptic label s t a b
         )
      => Optic (GetOpticKind s) '[] s t a b
optic = genOptic @label

field :: forall label s t a b
       . ( SameBase s t
         , GenOptic label s t a b
         , GetOpticKind s ~ L.A_Lens
         )
      => L.Lens s t a b
field = optic @label

_Ctor :: forall label s t a b
       . ( SameBase s t
         , GenOptic label s t a b
         , GetOpticKind s ~ P.A_Prism
         )
      => P.Prism s t a b
_Ctor = optic @label

-- | Assert that types are the same modulo type arguments
type SameBase :: k -> k -> Constraint
type family SameBase s t where
  SameBase (s a) (t b) = SameBase s t
  SameBase s t = s ~ t

type GenOptic :: Symbol -> Type -> Type -> Type -> Type -> Constraint
class GenOptic label s t a b where
  genOptic :: Optic (GetOpticKind s) '[] s t a b

type GetOpticKind :: Type -> OpticKind
type family GetOpticKind s

mkLens :: forall s t a b. (s -> a) -> (s -> b -> t) -> L.Lens s t a b
mkLens = L.lens

mkPrism :: forall s t a b. (b -> t) -> (s -> Either t a) -> P.Prism s t a b
mkPrism = P.prism
