{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
module Optician.Types
  ( GenOptic(..)
  , GetOpticKind
  , A_Lens
  , A_Prism
  ) where

import           Data.Kind
import           GHC.TypeLits
import           Optics.Lens
import           Optics.Optic
import           Optics.Prism

type GenOptic :: Symbol -> Type -> Type -> Type -> Type -> Constraint
class GenOptic label s t a b where
  genOptic :: Optic (GetOpticKind s) '[] s t a b

type GetOpticKind :: Type -> OpticKind
type family GetOpticKind s
