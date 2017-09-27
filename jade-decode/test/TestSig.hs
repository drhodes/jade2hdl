module TestSig (testTree) where

import Jade.Types
import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Sig as Sig

import qualified Jade.Decode as Decode
import qualified Jade.Module as Modul
import qualified Jade.Net as Net
import qualified Jade.Wire as Wire
import Text.Format
import Control.Monad
import Jade.Rawr.Types 
import Jade.Util

--------------------------------------------------------------------------------
testSigWidth :: Monad m => Sig -> Integer -> m TestState
testSigWidth sig expectedWidth = do
  let w = Sig.width sig
  if w == expectedWidth
    then return Pass
    else return (Fail (fmt "expected {0}, got {1}, sig: {2}" (expectedWidth, w, sig)))
               
testTreeSigWidth :: TestTree
testTreeSigWidth = 
  let t sig exp = TestCase "sigwidth" (testSigWidth sig exp)
  in TestTree "width" $ [ t (SigSimple "asdf") 1
                        , t (SigRange "" 7 0) 8
                        , t (SigRange "" 0 7) 8
                        , t (SigRangeStep "" 31 0 1) 32
                        , t (SigRangeStep "" 28 0 4) 8
                        , t (SigRangeStep "" 0 0 0) 1
                        ] ++ [t (SigRangeStep "" i 0 1) (i+1) | i <- [0 .. 12]]

testTree = TestTree "Sig" [ testTreeSigWidth
                          , testTreeTwosComplement
                          ]

testTwosCompement val w exp = do
  let tc = Sig.twosComplement val w
  if tc == exp
    then return Pass
    else return (Fail (fmt "expected {0}, got {1}" (exp, tc)))

testTreeTwosComplement :: TestTree
testTreeTwosComplement = 
  let t val w exp = TestCase "twos complement" $ testTwosCompement val w exp
  in TestTree "twosComplement" $ [ t 0 1 [L]
                                 , t 1 1 [H]
                                 , t 0 2 [L, L]
                                 , t 1 2 [L, H]
                                 , t 2 2 [H, L]
                                 , t 3 2 [H, H]
                                 ]
