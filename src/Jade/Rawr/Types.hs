{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jade.Rawr.Types where
import Prelude hiding (pow)
import Control.Monad

import qualified Control.Parallel.Strategies as CPS

-- class Compose a b where
--   (·) a b c :: a -> b -> c

nofact n = if n <= 0
           then 0
           else n + (nofact (n-1))

test1 = CPS.parList CPS.rseq (map nofact $ take 10 (repeat 10000000))

data Case = Case String (IO TestState)
          | Done String TestState
          
data TestTree = TestTree String [TestTree]
              | TestNode Case

runTree :: TestTree -> IO [TestState]
runTree (TestTree s trees) = do
  putStr $ take 20 $ s ++ ":                    "
  result <- liftM concat $ mapM runTree trees
  putStrLn ""
  return result
  
runTree (TestNode (Case s f)) = do
  sequence $ CPS.runEval $ CPS.parList CPS.rseq [f]

data TestState = Ready
               | Running
               | Pass
               | Fail String               
               deriving (Show, Eq)

midpoint (a, b) = (a + b) `div` 2

{-
type R = (Integer, Integer)

factor' :: Integer -> Integer -> R -> R -> Integer -> Integer -> R
factor' step n rangeA rangeB a b =
  if step > 1000 then (0, 0)
  else if a * b == n
       then (a, b)
       else if a * b < n
            then factor' (step + 1) n rangeA (midpoint rangeB, n) a (midpoint rangeB)
            else factor' (step + 1) n (a, midpoint rangeA) rangeB (midpoint rangeA) b



factor :: Integer -> (Integer, Integer)
factor n = let g = floor $ sqrt (fromIntegral n)
           in factor' 0 n (0, g) (g+1, n) g g


add x y = x + y
mul x y = x * y

pow x 1 = x
pow x y = x * (pow x (y - 1))

upto' 0 a b = Upto 0 a b
upto' n a (N 0) = upto' (n - 1) a (N 0)
upto' n a (N b) = Upto (n - 1) a (upto' n a (N (b - 1)))

upto n a b = upto' n (N a) (N b)

data Upto = Upto Integer Upto Upto
          | N Integer
            deriving (Show)

tupo :: Integer -> Integer -> Integer -> Integer
tupo 0 a b = a + b
tupo n 0 _ = 0
tupo n _ 0 = 0
tupo 1 a b = a * b
tupo 2 a b = a ^ b
tupo n a b = foldl (tupo (n-1)) a (take (fromIntegral (b-1)) (repeat a))

commutesOneP f a b = f a b == f b a
associatesOneP f a b c = (f (f a b) c) == (f a (f b c)) 

commutesSomeP f = let ns = [1 .. 4]
                  in and [commutesOneP f x y | x <- ns, y <- ns]

associatesSomeP f = let ns = [1 .. 3]
                    in and [associatesOneP f x y z | x <- ns, y <- ns, z <- ns]

n0 = []
n1 = n0:n0
n2 = n0:n1
n3 = n0:n2
n4 = n0:n3
n5 = n0:n4
n6 = n0:n5

divide xs ys = 


myfold f (x:[]) = x
myfold f (x:y:rest) = let result = f x y
                      in myfold f (result:rest)

listn 0 xs ys = xs ++ ys
listn 1 xs ys = concat [xs | _ <- ys]
listn n xs ys = let f = listn (n - 1)
                    items = [xs | _ <- ys]
                in myfold f items

--tupo n a b = foldl (tupo (n-1)) a (take (fromIntegral (b-1)) (repeat a))
  
log2 x = log x / log 2
-}
