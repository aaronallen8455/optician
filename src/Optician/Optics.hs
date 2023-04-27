-- | This module re-exports everything that the @Optics@ module does except for
-- @Optics.Label@, which is not compatible with the @Optician.Label@ module.
-- Thus, you should import this module instead of @Optics@ if you want to use
-- @Optician.Label@.
module Optician.Optics
  ( module Optics.Optic
  , module O
  , module Optics.Operators
  , module Optics.At
  , module Optics.Cons
  , module Optics.Each
  , module Optics.Empty
  , module Optics.Re
  , module Optics.ReadOnly
  , module Optics.Mapping
  , module Optics.State
  , module Optics.View
  , module Optics.Zoom
  , module Optics.Indexed
  , module P
  ) where

import Optics.Optic

import Optics.Traversal                      as O
import Optics.Setter                         as O
import Optics.Review                         as O
import Optics.ReversedPrism                  as O
import Optics.Prism                          as O
import Optics.ReversedLens                   as O
import Optics.Lens                           as O
import Optics.IxTraversal                    as O
import Optics.IxSetter                       as O
import Optics.IxFold                         as O
import Optics.IxAffineTraversal              as O
import Optics.IxAffineFold                   as O
import Optics.IxGetter                       as O
import Optics.IxLens                         as O
import Optics.Iso                            as O
import Optics.Getter                         as O
import Optics.Fold                           as O
import Optics.AffineTraversal                as O
import Optics.AffineFold                     as O

import Optics.At
import Optics.Cons
import Optics.Each
import Optics.Empty
import Optics.Indexed
import Optics.Mapping
import Optics.Operators
import Optics.Re
import Optics.ReadOnly
import Optics.State
import Optics.View
import Optics.Zoom

import Data.Tuple.Optics                     as P
import Data.Maybe.Optics                     as P
import Data.Either.Optics                    as P
