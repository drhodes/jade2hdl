{-# LANGUAGE FlexibleInstances #-}
module Jade.Arbitrary where

import Jade.Types
import Test.QuickCheck.Arbitrary
import qualified Test.QuickCheck.Property as TQP
import Test.QuickCheck
import Control.Monad
import Data.Word as DW
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
number n = read <$> replicateM n digit

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
  arbitrary = Signal <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Val where
  arbitrary = ValIndex <$> arbitrary <*> arbitrary

instance Arbitrary (Bundle Val) where
  arbitrary = Bundle <$> arbitrary

arb2 x = x <$> arbitrary <*> arbitrary    

instance Arbitrary Wire where arbitrary = arb2 Wire
instance Arbitrary Port where arbitrary = arb2 Port

instance Arbitrary SubModule where
  arbitrary = SubModule <$> word 10 <*> arbitrary

instance Arbitrary Jumper where
  arbitrary = Jumper <$> arbitrary

instance Arbitrary Part where
  arbitrary = join $ oneOf [ PortC <$> arbitrary
                           , SubModuleC <$> arbitrary
                           , WireC  <$> arbitrary
                           , JumperC <$> arbitrary
                           ]



prop xs = (reverse xs) == xs
  where types = xs :: [Int]

-- test group properties of color pixel.

class Zeroish a where
  hasZero :: a -> Bool

data Pixel = Pixel { pxR :: DW.Word8
                   , pxG :: DW.Word8
                   , pxB :: DW.Word8
                   } deriving (Show, Eq)

instance Arbitrary Pixel where
  arbitrary = Pixel <$> arbitrary <*> arbitrary <*> arbitrary


pixelAdd (Pixel r1 g1 b1) (Pixel r2 g2 b2) = Pixel (r1+r2) (g1+g2) (b1+b2)
pixelSub (Pixel r1 g1 b1) (Pixel r2 g2 b2) = Pixel (r1-r2) (g1-g2) (b1-b2)


hasZeroProp (p1, p2) = p1 `pixelAdd` p2 == p1
hasZeroProp1 p = (Pixel 0 0 0) `pixelAdd` p == p

notHasZeroProp (p1, p2)  = not $ hasZeroProp (p1, p2)

testForZero = quickCheckWith stdArgs{maxSuccess = 1000} (forAll (arbitrary :: Gen (Pixel)) hasZeroProp1)

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs


hasCommutative op (p1, p2) = let px1 = op p1 p2
                                 px2 = op p2 p1
                             in px1 == px2

hasAssociative op (p1, p2, p3) = let px1 = op (op p1 p2) p3
                                     px2 = op p1 (op p2 p3)
                                 in px1 == px2



testForPropFunc prop =
  quickCheckWith (stdArgs{maxSuccess = 1000}) (forAll arbitrary prop)



testForCommutativeAdd = testForPropFunc $ hasCommutative pixelAdd
testForAssociativeAdd = testForPropFunc $ hasAssociative pixelAdd

testForCommutativeSub = testForPropFunc $ hasCommutative pixelSub
testForAssociativeSub = testForPropFunc $ hasAssociative pixelSub


testAll = do
  mapM testForPropFunc [ hasCommutative pixelAdd
                       , hasCommutative pixelSub ]
  mapM testForPropFunc [ hasAssociative pixelAdd
                       , hasAssociative pixelSub ]
