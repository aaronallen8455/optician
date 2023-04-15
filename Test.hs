{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin Optician #-}

import           Optician
import           Optician.Label
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
test = #a -- optic @"a" -- lens a $ \x a -> x { a = a }

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

otherLens :: Lens' Exist Int
otherLens = lens other (\(Exist x o) b -> Exist x b)

data Exist2 f x where
  Exist2 :: forall y x f. (Show x, Show y) => { exf2 :: f x, other2 :: y } -> Exist2 f x

exist2 :: Exist2 [] ()
exist2 = Exist2 [()] "..."

data Exist3 x where
  Exist3 :: { exf3 :: String } -> Exist3 String

exist3 :: Exist3 String
exist3 = Exist3 ".."

data Plain = Plain Int Bool

plain :: Plain
plain = Plain 12 True

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

data Su2 a
  = Su21 (Maybe a)
  | Su22 Int (Maybe (Su2 Double))
  | Su23 Foo
  deriving Show

p4 :: Prism (Su2 ()) (Su2 Int) (Maybe ()) (Maybe Int)
p4 = _Ctor @"Su21"

p5 :: Prism (Su2 a) (Su2 a) (Maybe a) (Maybe a)
p5 = _Ctor @"Su21"


-- p5 :: Prism (Su2 ()) (Su2 Int) Int Int
-- p5 = _Ctor @"Su22"

p6 :: AffineTraversal' (Su2 Double) Int
p6 = #Su22 % _2 % #Just % #Su22 % _2 % #Just % #Su23 % #a
