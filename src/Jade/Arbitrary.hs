module Jade.Arbitrary where

import Jade.Types
import Test.QuickCheck.Arbitrary
import Test.QuickCheck
import Control.Monad
import qualified Jade.Sig as Sig

oneOf xs = do
  if (length xs == 0)
    then fail ("oneOf can't take an empty list")
    else do n <- arbitrary
            return $ xs !! (n `mod` (length xs))

letter :: Gen Char
letter = oneOf "abcdefghijklmnopqrstuvwxyz"

word :: Int -> Gen [Char]
word n = replicateM n letter

digit :: Gen Char
digit = oneOf "0123456789"

number :: Int -> Gen Integer
number n = liftM read $ replicateM n digit

instance Arbitrary Direction where
  arbitrary = oneOf [In, Out]

instance Arbitrary Line where
  arbitrary = Line <$> arbitrary

instance Arbitrary Rot where
  arbitrary = oneOf $ map toEnum [0 .. 7]

instance Arbitrary Coord5 where
  arbitrary = Coord5 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Coord3 where
  arbitrary = Coord3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Sig where
  arbitrary = do
    name <- word 5
    a <- number 2
    b <- number 2
    c <- number 1
    oneOf [ SigSimple name
          , SigIndex name a
          , SigHash name a
          , SigRange name a b
          , SigRangeStep name a b c
          , SigQuote a b
          ]

instance Arbitrary Signal where
  arbitrary = do
    sig <- arbitrary
    let w = show $ Sig.width sig
    dir <- liftM Just arbitrary
    return $ Signal (Just sig) (Just w) dir

arb2 x = x <$> arbitrary <*> arbitrary    

instance Arbitrary Wire where arbitrary = arb2 Wire
instance Arbitrary Port where arbitrary = arb2 Port

instance Arbitrary SubModule where
  arbitrary = SubModule <$> word 10 <*> arbitrary

instance Arbitrary Jumper where
  arbitrary = Jumper <$> arbitrary

instance Arbitrary Component where
  arbitrary = join $ oneOf [ PortC <$> arbitrary
                           , SubModuleC <$> arbitrary
                           , WireC  <$> arbitrary
                           , JumperC <$> arbitrary
                           ]


