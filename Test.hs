{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin Optician #-}

import           Optician
import           Optics.Optic
import           Optics.Lens
import           Optics.Operators

data Foo = Foo { a:: Int} deriving Show

test :: Optic A_Lens '[] Foo Foo Int Int
test = optic @"a" -- lens a $ \x a -> x { a = a }
