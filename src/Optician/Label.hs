{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Exposes an orphan instance that allows overloaded labels to be used for
-- lenses. Currently an instance cannot be added for prisms due to the fun deps
-- on 'LabelOptic'.
-- It's possible to write an 'IsLabel' instance for 'Optic' directly but then
-- it clashes with instance from Optic.Label which is exposed by numerous
-- modules from the optics package.
module Optician.Label
  (
  ) where

import           GHC.Records
import           Optics.Label
import           Optics.Lens

import           Optician.Internal (GenOptic, GetOpticKind, optic, SameBase, GenTypeEqualities)

-- orphan
instance {-# OVERLAPS #-}
         ( GenOptic label s t a b
         , SameBase s t
         , GenTypeEqualities label s t a b
         , GetOpticKind label s ~ A_Lens
         , HasField label s a
         , HasField label t b
         )
  => LabelOptic label A_Lens s t a b where
    labelOptic = optic @label

-- instance ( GenOptic label s t a b
--          , is ~ NoIx
--          , k ~ GetOpticKind label s
--          , SameBase s t
--          , GenTypeEqualities label s t a b
--          )
--   => IsLabel label (Optic k is s t a b) where
--     fromLabel = optic @label
