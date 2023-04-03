{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import           Control.Lens.Properties
import           GHC.Generics
import           Optics.Optic
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
    -- TODO add tests for polymorphic records
    -- TODO add tests for existentials
    -- TODO add tests for contexts
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
