{-# LANGUAGE DataKinds #-}

module Main (main) where

import           Control.Lens.Properties
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
            $ isLens (toLensVL $ field @"t1" @Basic)
          .&. isLens (toLensVL $ field @"t2" @Basic)
          .&. isLens (toLensVL $ field @"t3" @Basic)
          .&. isLens (toLensVL $ field @"t4" @Basic)
      , testGroup "prisms"
        [
        ]
      ]
    -- TODO add tests for polymorphic records
    -- TODO add tests for existentials
    -- TODO add tests for contexts
    ]

data Basic = Basic
  { t1 :: Int
  , t2 :: Maybe Bool
  , t3 :: Double
  , t4 :: [()]
  } deriving (Show, Eq)

instance Arbitrary Basic where
  arbitrary = Basic
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
