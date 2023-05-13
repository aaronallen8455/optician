{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, OverloadedStrings, GADTs, ViewPatterns #-}
{-# LANGUAGE DatatypeContexts #-}

import           Control.Exception
import qualified Data.Text as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Optician
import           Optics
import           System.IO

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup "optic type error tests"
    [ testCase "lens missing field" lensMissingField
    , testCase "data con not in scope" dataConNotInScope
    , testCase "selector has existential" selectorHasExistential
    , testCase "data con without fields" dataConWithoutFields
    , testCase "type does not have data con" typeDoesntHaveDataCon
    , testCase "prism data con has existentials" dataConHasExistentials
    , testCase "data con with stupid theta" dataConWithStupidTheta
    , testGroup "constraint context"
      [ testCase "lens" contextLens
      , testCase "prism" contextPrism
      ]
    , testGroup "cannot change phantom type"
      [ testCase "lens" phantomLens
      , testCase "prism" phantomPrism
      ]
    , testGroup "polymorphic optics"
      [ testCase "lens cannot change ty var of unfocused field"
          $ unfocusedPolyFieldLens1
         >> unfocusedPolyFieldLens2
      , testCase "prism cannot change ty var used in unfocused con" unfocusedTyVarPrism
      ]
    ]

data Rec1 = Rec1
  { r1 :: Int
  , r2 :: Bool
  }

rec1 :: Rec1
rec1 = Rec1 9 True

lensMissingField :: Assertion
lensMissingField = do
  let l = field @"r3" @Rec1
  checkTypeError "'Rec1' does not have field \"r3\"" (rec1 ^. l)

dataConNotInScope :: Assertion
dataConNotInScope = do
  let p = _Ctor @"FileHandle" @Handle
  checkTypeError "Data constructor not in scope: \"FileHandle\"" (stdin ^? p)

data Exist where
  Exist :: { ex1 :: a } -> Exist

exist :: Exist
exist = Exist ()

selectorHasExistential :: Assertion
selectorHasExistential = do
  let l = field @"ex1" @Exist
  checkTypeError "Cannot make a lens for a field with an existential type"
    $ Exist True ^. l

  checkTypeError "Cannot make a lens for a field with an existential type"
    $ exist & field @"ex1" .~ True

data Rec2 = Rec2 Bool String

dataConWithoutFields :: Assertion
dataConWithoutFields = do
  let l = field @"x" @Rec2
  checkTypeError "Cannot make a lens for a data constructor that does not have fields: \"Rec2\""
    $ Rec2 True "" ^. l

typeDoesntHaveDataCon :: Assertion
typeDoesntHaveDataCon = do
  let p = _Ctor @"Nothin"
  checkTypeError "Type 'Maybe Bool' does not have data constructor: \"Nothin\""
    $ Just True ^? p

data ExistSum where
  ES1 :: a -> ExistSum
  ES2 :: Int -> ExistSum

dataConHasExistentials :: Assertion
dataConHasExistentials = do
  checkTypeError "Cannot make a prism for a data constructor with an existential type"
    $ ES1 () ^? _Ctor @"ES1"

  checkTypeError "Cannot make a prism for a data constructor with an existential type"
    (review (_Ctor @"ES1") () :: ExistSum)

data Show a => StupidTheta a = StupidTheta { st1 :: a, st2 :: Bool }

dataConWithStupidTheta :: Assertion
dataConWithStupidTheta =
  checkTypeError "Cannot make an optics for data constructors with a 'stupid theta'"
    $ StupidTheta () True ^. field @"st1"

data Rec3 a = Rec3 { r31 :: a, r32 :: a }

rec3Lens :: Lens (Rec3 Bool) (Rec3 ()) Bool ()
rec3Lens = field @"r31"

unfocusedPolyFieldLens1 :: Assertion
unfocusedPolyFieldLens1 =
  checkTypeError "Couldn't match type ‘Bool’ with ‘()’"
    $ Rec3 True False & rec3Lens .~ ()

data Rec4 a b = Rec4 { r41 :: a, r42 :: b }

rec4Lens :: Lens (Rec4 Bool Bool) (Rec4 Int Int) Bool Int
rec4Lens = field @"r41"

unfocusedPolyFieldLens2 :: Assertion
unfocusedPolyFieldLens2 =
  checkTypeError "Couldn't match type ‘Bool’ with ‘Int’"
    $ Rec4 True False & rec4Lens .~ 9

data Phan a = Phan { p1 :: Int, p2 :: Bool }

phan :: Lens (Phan Int) (Phan String) Int Int
phan = field @"p1"

phantomLens :: Assertion
phantomLens =
  checkTypeError "Couldn't match type ‘Int’ with ‘[Char]’"
    $ Phan 1 True & phan .~ 9

data PhanSum a = PS1 Bool | PS2 Int

phanP :: Prism (PhanSum Int) (PhanSum String) Bool Bool
phanP = _Ctor @"PS1"

phantomPrism :: Assertion
phantomPrism =
  checkTypeError "Couldn't match type ‘Int’ with ‘[Char]’"
    $ PS1 True & phanP .~ False

data Ctx1 a where
  Ctx1 :: Show a => { c11 :: a } -> Ctx1 a

contextLens :: Assertion
contextLens =
#if MIN_VERSION_ghc(9,6,0)
  checkTypeError "No instance for ‘Show (Ctx1 ())’ arising from a use of ‘field’" $
#elif MIN_VERSION_ghc(9,4,0)
  checkTypeError "No instance for (Show (Ctx1 ())) arising from a use of ‘field’" $
#endif
    case Ctx1 () & field @"c11" .~ Ctx1 () of
      Ctx1 a -> show a

data Ctx2 a where
  Ctx21 :: Ord a => a -> Ctx2 a
  Ctx22 :: Ctx2 Bool

contextPrism :: Assertion
contextPrism =
#if MIN_VERSION_ghc(9,6,0)
  checkTypeError "No instance for ‘Ord (Ctx2 ())’ arising from a use of ‘_Ctor’" $
#elif MIN_VERSION_ghc(9,4,0)
  checkTypeError "No instance for (Ord (Ctx2 ())) arising from a use of ‘_Ctor’" $
#endif
    case Ctx21 () & _Ctor @"Ctx21" .~ Ctx21 () of
      Ctx21 a -> a > a

data PolySum1 a = PoS1 a | PoS2 a

polyPrism :: Prism (PolySum1 Bool) (PolySum1 ()) Bool ()
polyPrism = _Ctor @"PoS1"

unfocusedTyVarPrism :: Assertion
unfocusedTyVarPrism =
  checkTypeError "Couldn't match type ‘Bool’ with ‘()’"
    $ PoS1 True & polyPrism .~ ()

checkTypeError :: T.Text -> a -> Assertion
checkTypeError subString v = do
  e <- try @SomeException $ evaluate v
  case e of
    Left (T.breakOn "In the expression" . T.pack . show -> (err, _)) -> do
      assertBool ("check type error: " ++ T.unpack err) $
        subString `T.isInfixOf` err
    Right _ -> assertFailure "No type error"
