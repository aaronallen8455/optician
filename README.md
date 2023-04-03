# Optician :eyeglasses:

This is a GHC plugin that generates lenses and prisms without the need for
template haskell or generics. It is intended to be used with the `optics`
library although support for `lens` could be added in the future.

:warning: **work in progress**

## Example
```haskell
{-# OPTIONS_GHC -fplugin Optician #-}
{-# LANGUAGE DataKinds #-}

import Optician
import Optics

data Person = MkPerson
  { name :: String
  , age :: Int
  }

incrementAge :: Person -> Person
incrementAge = field @"age" %~ succ

data Pet
  = Dog String
  | Cat String

setDogName :: Pet -> String -> Pet
setDogName pet newName = pet & _Ctor @"Dog" .~ newName
```
