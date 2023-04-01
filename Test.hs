{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin Optician #-}

import           Optician
import           Optics.Optic
import           Optics.Lens
import           Optics.Operators
import           Optics.Prism
import           Optics.AffineTraversal
import           Data.Tuple.Optics

f :: Foo
f = Foo 9 "a"

data Foo = Foo { a:: Int, aa :: String} deriving Show

test :: Optic A_Lens '[] Foo Foo Int Int
test = optic @"a" -- lens a $ \x a -> x { a = a }

data Bar a b = Bar { b :: a
                   , c :: a
                   , d :: Int
                   } deriving Show

bar :: Bar Int ()
bar = Bar 9 10 8

data Baz a b where
  Baz :: forall b a. Show b => { e :: a
                     , e2 :: b
                     } -> Baz a b

baz :: Baz Int ()
baz = Baz 3 ()

data Exist where
  Exist :: Show x => { exf :: Either Int x, other :: Int } -> Exist

exist :: Exist
exist = Exist (Right "test") 9

repack :: Exist -> Exist
repack Exist { exf = a, other = b } = Exist a $ b + 1

-- what about GADTs with constraint contexts?

data Su
  = Su1 Int
  | Su2 Double String
  deriving Show

p :: Prism' Su Int
p = _Ctor @"Su1"

su = Su1 9
su2 = Su2 9.1 "..."

p2 :: Prism' Su (Double, String)
p2 = _Ctor @"Su2"

p3 :: AffineTraversal' Su Double
p3 = p2 % _1

-- evaluating
-- su2 & _Ctor @"Su2" % _2 .~ "yes"
-- causes a crash. Emit wanteds without evidence if types don't match?
