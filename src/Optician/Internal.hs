{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
module Optician.Internal
  ( GenOptic
  , optic
  , field
  , _Ctor
  , GetOpticKind
  , ALens
  , APrism
  , mkLens
  , mkPrism
  , GenTypeEqualities
  , SameBase
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
         , GenTypeEqualities label s t a b
         )
      => Optic (GetOpticKind label s) NoIx s t a b
optic = genOptic @label

-- | Lens for a field in a record.
--
-- @since 0.1.0.0
field :: forall label s t a b
       . ( SameBase s t
         , GenOptic label s t a b
         , GetOpticKind label s ~ L.A_Lens
         , GenTypeEqualities label s t a b
         )
      => L.Lens s t a b
field = optic @label

-- | Prism for a sum type.
--
-- @since 0.1.0.0
_Ctor :: forall label s t a b
       . ( SameBase s t
         , GenTypeEqualities label s t a b
         , GenOptic label s t a b
         , GetOpticKind label s ~ P.A_Prism
         )
      => P.Prism s t a b
_Ctor = optic @label

-- | Assert that types are the same modulo type arguments
type SameBase :: k -> k -> Constraint
type family SameBase s t where
  SameBase (s a) (t b) = SameBase s t
  SameBase s t = s ~ t

type GenTypeEqualities :: Symbol -> Type -> Type -> Type -> Type -> Constraint
type family GenTypeEqualities label s t a b where

type GenOptic :: Symbol -> Type -> Type -> Type -> Type -> Constraint
class GenOptic label s t a b
    | s label -> a
    , t label -> b
    , s label b -> t
    , t label a -> s where
  genOptic :: Optic (GetOpticKind label s) NoIx s t a b

type GetOpticKind :: Symbol -> Type -> OpticKind
type family GetOpticKind label s where

mkLens :: forall s t a b. (s -> a) -> (s -> b -> t) -> L.Lens s t a b
mkLens = L.lens

mkPrism :: forall s t a b. (b -> t) -> (s -> Either t a) -> P.Prism s t a b
mkPrism = P.prism
