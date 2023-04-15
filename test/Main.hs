{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main, Exist(..), ExistP(..), Ctx(..)) where

import           Control.Lens.Properties
import           GHC.Generics
import           Optics
import           Optics.VL
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Optician

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup "optic tests"
    [ testGroup "basic"
      [ testProperty "lens"
            $ isLens (toLensVL $ field @"t1" @BasicRec)
          .&. isLens (toLensVL $ field @"t2" @BasicRec)
          .&. isLens (toLensVL $ field @"t3" @BasicRec)
          .&. isLens (toLensVL $ field @"t4" @BasicRec)
      , testProperty "lens composition"
          $ isLens (toLensVL $ field @"t4" @BasicRec % field @"ir1")
      , testProperty "prism"
            $ isPrism (toPrismVL $ _Ctor @"BS1" @BasicSum)
          .&. isPrism (toPrismVL $ _Ctor @"BS2" @BasicSum)
          .&. isPrism (toPrismVL $ _Ctor @"BS3" @BasicSum)
      , testProperty "prism composition"
          $ isPrism (toPrismVL $ _Ctor @"BS1" @BasicSum % _Ctor @"Just" @(Maybe InnerRec))
      ]
    , testGroup "polymorphic"
      [ testProperty "lens"
          $ isLens (toLensVL $ pr1Lens @Int)
        .&. polyRecLensTest
        .&. polyRec2LensTest
      , testProperty "prism"
          $ isPrism (toPrismVL $ ps1Prism @Int @Double @Bool)
        .&. ps1Prop
        .&. ps2Prop
      ]
    , testGroup "existentials"
      [ testProperty "lens"
          $ existLensTest
        .&. existPLensTest
        -- TODO prism
      ]
    , testGroup "contexts"
      [ testProperty "lens"
          $ ctxLensTest1
        .&. ctxLensTest2
        -- TODO prism
      ]
    ]

data BasicRec = BasicRec
  { t1 :: Int
  , t2 :: Maybe Bool
  , t3 :: Double
  , t4 :: InnerRec
  } deriving (Show, Eq)

instance Arbitrary BasicRec where
  arbitrary = BasicRec
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

data InnerRec = InnerRec
  { ir1 :: Int
  , ir2 :: Double
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (CoArbitrary, Function)

instance Arbitrary InnerRec where
  arbitrary = InnerRec
    <$> arbitrary
    <*> arbitrary

data PolyRec a = PolyRec
  { pr1 :: a
  , pr2 :: Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (CoArbitrary, Function)

instance Arbitrary a => Arbitrary (PolyRec a) where
  arbitrary = PolyRec <$> arbitrary <*> arbitrary

pr1Lens :: Lens' (PolyRec a) a
pr1Lens = field @"pr1"

pr1Lens' :: Lens (PolyRec a) (PolyRec b) a b
pr1Lens' = field @"pr1"

polyRecLensTest :: Property
polyRecLensTest =
  forAll (arbitrary @(PolyRec Int)) $ \pr ->
    forAll (arbitrary @Double) $ \dbl ->
      view pr1Lens' (set pr1Lens' dbl pr) === dbl
      .&. set pr1Lens' (view pr1Lens' pr) pr === pr
      .&. set pr1Lens' dbl (set pr1Lens' dbl pr) === set pr1Lens' dbl pr

data PolyRec2 a b = PolyRec2
  { pr21 :: [b]
  , pr22 :: Maybe a
  , pr23 :: (b, b)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (CoArbitrary, Function)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PolyRec2 a b) where
  arbitrary = PolyRec2 <$> arbitrary <*> arbitrary <*> arbitrary

pr22Lens :: Lens (PolyRec2 a b) (PolyRec2 c b) (Maybe a) (Maybe c)
pr22Lens = field @"pr22"

polyRec2LensTest :: Property
polyRec2LensTest =
  forAll (arbitrary @(PolyRec2 Int Bool)) $ \pr ->
    forAll (arbitrary @(Maybe Double)) $ \dbl ->
      view pr22Lens (set pr22Lens dbl pr) === dbl
      .&. set pr22Lens (view pr22Lens pr) pr === pr
      .&. set pr22Lens dbl (set pr22Lens dbl pr) === set pr22Lens dbl pr

data BasicSum
  = BS1 (Maybe InnerRec)
  | BS2 Bool Double
  | BS3 () Int (Maybe Bool)
  deriving (Show, Eq)

instance Arbitrary BasicSum where
  arbitrary = oneof
    [ BS1 <$> arbitrary
    , BS2 <$> arbitrary <*> arbitrary
    , BS3 <$> arbitrary <*> arbitrary <*> arbitrary
    ]

data PolySum a b c
  = PS1 a
  | PS2 b c
  deriving stock (Show, Eq, Generic)
  deriving anyclass (CoArbitrary, Function)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (PolySum a b c) where
  arbitrary = oneof
    [ PS1 <$> arbitrary
    , PS2 <$> arbitrary <*> arbitrary
    ]

ps1Prism :: Prism' (PolySum a b c) a
ps1Prism = _Ctor @"PS1"

ps1Prism' :: Prism (PolySum a b c) (PolySum z b c) a z
ps1Prism' = _Ctor @"PS1"

ps1Prop :: Property
ps1Prop =
  forAll (arbitrary @(PolySum Int Double Bool)) $ \ps ->
    forAll (arbitrary @(Maybe Word)) $ \v ->
      preview ps1Prism' (review ps1Prism' v) === Just v
      .&. maybe discard (\x -> review ps1Prism' x === ps) (preview ps1Prism' ps)

ps2Prism :: Prism (PolySum a b c) (PolySum a x y) (b, c) (x, y)
ps2Prism = _Ctor @"PS2"

ps2Prop :: Property
ps2Prop =
  forAll (arbitrary @(PolySum Int Double Bool)) $ \ps ->
    forAll (arbitrary @(Maybe Word, Int)) $ \v ->
      preview ps2Prism (review ps2Prism v) === Just v
      .&. maybe discard (\x -> review ps2Prism x === ps) (preview ps2Prism ps)

data Exist where
  Exist :: { ex1 :: Int, ex2 :: a } -> Exist

instance Show Exist where
  show _ = "exist"

instance Eq Exist where
  Exist a _ == Exist c _ = a == c

exist :: Exist
exist = Exist 9 True

existLensTest :: Property
existLensTest =
  forAll (arbitrary @Int) $ \i ->
    view (field @"ex1") (set (field @"ex1") i exist) === i
    .&. set (field @"ex1") (view (field @"ex1") exist) exist === exist
    .&. set (field @"ex1") i (set (field @"ex1") i exist) === set (field @"ex1") i exist

data ExistP a where
  ExistP :: { exp1 :: a, exp2 :: x } -> ExistP a

instance Show a => Show (ExistP a) where
  show (ExistP a _) = show a

instance Eq a => Eq (ExistP a) where
  ExistP a _ == ExistP c _ = a == c

existP :: ExistP Double
existP = ExistP 9 True

existPLensTest :: Property
existPLensTest =
  forAll (arbitrary @Int) $ \i ->
    view (field @"exp1" @(ExistP Int)) (set (field @"exp1") i existP) === i
    .&. set (field @"exp1") (view (field @"exp1") existP) existP === existP
    .&. set (field @"exp1") i (set (field @"exp1") i existP) === set (field @"exp1") i existP

data Ctx a b where
  Ctx :: (Show a, Eq b) => { cf1 :: a, cf2 :: b } -> Ctx a b

instance (Show a, Show b) => Show (Ctx a b) where
  show (Ctx a b) = show (a, b)

instance (Eq a, Eq b) => Eq (Ctx a b) where
  Ctx a b == Ctx c d = (a, b) == (c, d)

ctx :: Ctx Bool Int
ctx = Ctx True 9

ctxLensTest1 :: Property
ctxLensTest1 =
  forAll (arbitrary @(Maybe Int)) $ \i ->
    view (field @"cf1" @(Ctx (Maybe Int) Int)) (set (field @"cf1") i ctx) === i
    .&. set (field @"cf1") (view (field @"cf1") ctx) ctx === ctx
    .&. set (field @"cf1") i (set (field @"cf1") i ctx) === set (field @"cf1") i ctx

ctxLensTest2 :: Property
ctxLensTest2 =
  forAll (arbitrary @(Maybe Bool)) $ \i ->
    view (field @"cf2" @(Ctx Bool (Maybe Bool))) (set (field @"cf2") i ctx) === i
    .&. set (field @"cf2") (view (field @"cf2") ctx) ctx === ctx
    .&. set (field @"cf2") i (set (field @"cf2") i ctx) === set (field @"cf2") i ctx
