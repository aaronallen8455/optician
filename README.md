# Optician :eyeglasses:

This is a GHC plugin that generates lenses and prisms without the need for
template haskell or generics. It is intended to be used with the [`optics`](https://hackage.haskell.org/package/optics)
library although support for `lens` could be added in the future.

## Example
```haskell
{-# OPTIONS_GHC -fplugin Optician #-}
{-# LANGUAGE DataKinds, TypeApplications #-}

import Optician (field, _Ctor)
import Optics (over, preview)

data Person = MkPerson
  { name :: String
  , age :: Int
  }

incrementAge :: Person -> Person
incrementAge = over (field @"age") (+1)

data Pet
  = Dog String
  | Cat String

getDogName :: Pet -> Maybe String
getDogName = preview (_Ctor @"Dog")
```

## Features
 - Exceptional ergonomics - optics are automatically available for in-scope
   data constructors
 - Improved compilation speed compared to Generics and Template Haskell based
   optics solutions
 - Supports polymorphic updates
 - Custom error messages
 - Fully compatible with GADTs
 - Good type inference

## Quick-start guide
 1. Add the `optician` package as a dependency of your project.
 2. Pass the `-fplugin Optician` argument to GHC to load the plugin. This can
    be done project wide by adding `ghc-options: -fplugin Optician` to your
    `*.cabal` or `project.yaml` file, or on a per module basis using the
    `{-# OPTIONS_GHC -fplugin Optician #-}` pragma.
 3. Import the `Optician` module and use `field` and `_Ctor` to generate lenses
    and prisms respectively. They both require a type-level string argument
    correspondind to the field or data constructor name which can be given
    using the `TypeApplications` extension (see example above).

## Support for `OverloadedLabels`

You can use the `OverloadedLabels` extension to generate optics instead of
`field` and `_Ctor` for greater terseness. To do this, import the
`Optician.Label` module and write `#fieldName` for a lens or `#ConstructorName`
for a prism (GHC 9.6 and above only). For example:

```haskell
{-# LANGUAGE OverloadedLabels #-}

import Optician.Label
import Optician.Optics ((%), AffineTraversal')

data Foo = Foo { foo :: Bar }
data Bar = Bar { bar :: Maybe Bool }

fooBar :: AffineTraversal' Foo Bool
fooBar = #foo % #bar % #Just
```

> Note: `Optician.Label` cannot be imported in conjunction with the `Optics`
> module (which re-exports `Optics.Label`) because of the conflicting `IsLabel`
> instance. Instead you should import `Optician.Optics`, which does not
> re-export `Optics.Label` but has most other exports.

## Limitations
 - Currently only GHC 9.4.x and 9.6.x are supported
