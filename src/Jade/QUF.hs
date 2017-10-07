{-# LANGUAGE FlexibleContexts #-}
module Jade.QUF where

import Data.Maybe
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array
import Data.STRef
import qualified Data.Map as DM
import qualified Data.List as DL
import Prelude hiding (id)
import qualified Data.List.Split as Split

data QUF s = QUF { parent :: STUArray s Int Int
                 , size :: STUArray s Int Int
                 , count :: Int
                 } 

new :: Int -> ST s (QUF s)
new n = do
  p <- newListArray (0, n) [0 .. n-1]
  s <- newArray (0, n) 1
  return $ QUF p s n

find :: QUF s -> Int -> ST s Int
find quf p = do
  validate quf p
  el <- readArray (parent quf) p
  if p == el
    then return p
    else find quf el

connected :: QUF s -> Int -> Int -> ST s Bool
connected quf p q = liftM2 (==) (find quf p) (find quf q)

union :: QUF s -> Int -> Int -> ST s (QUF s)
union quf p q = do
  rootP <- find quf p
  rootQ <- find quf q
  if (rootP == rootQ)
    then return quf -- don't mutate anything.
    else
    do sizeOfRootP <- readArray (size quf) rootP
       sizeOfRootQ <- readArray (size quf) rootQ
       if sizeOfRootP < sizeOfRootQ
         then do writeArray (parent quf) rootP rootQ
                 writeArray (size quf) rootQ (sizeOfRootQ + sizeOfRootP)
                 return quf{count = count quf - 1}
         else do writeArray (parent quf) rootQ rootP
                 writeArray (size quf) rootP (sizeOfRootQ + sizeOfRootP)
                 return quf{count = count quf - 1}

validate :: QUF s -> Int -> ST s ()
validate quf p = do
  (b1, b2) <- getBounds $ parent quf
  let n = b2-b1
  if p < 0 || p >= n
    then fail $ "index " ++ show p ++ " is not between 0 and " ++ show (n - 1)
    else return ()

unionMany :: QUF s -> [(Int, Int)] -> ST s (QUF s)
unionMany quf [] = return quf
unionMany quf ((p, q):rest) = do
  quf' <- union quf p q
  unionMany quf' rest
  
testQUF filename = do
  testLines <- lines <$> readFile filename
  let numInputs = read $ head testLines
  let pairs = [ (read x, read y) :: (Int, Int)
              | [x, y] <- map (Split.splitOn " ") (take numInputs $ drop 1 testLines)]
  
  let c = runST $ do
        quf <- new (length pairs)        
        quf' <- unionMany quf pairs
        allTrue <- and <$> sequence [connected quf' p q | (p, q) <- pairs]
        return $ (allTrue, count quf')

  print ("num pairs", length pairs)
  print c

oneNeighborhood quf elId remaining = filterM (connected quf elId) remaining

allNeighbors quf [] = return []
allNeighbors quf (uid:uniqIds) = do
  nbrhood <- oneNeighborhood quf uid uniqIds
  let remaining = uniqIds DL.\\ nbrhood
  ((uid : nbrhood):) <$> allNeighbors quf remaining

reverseMap m = DM.fromList [(y,x) | (x, y) <- DM.toList m]

components :: (Eq a, Ord a) => [(a, a)] -> [[a]]
components pairs = do
  let uniqEls     = DL.nub $ concat [[p1, p2] | (p1, p2) <- pairs]
      uniqIds     = [0 .. length uniqEls - 1]
      elemIdTable = DM.fromList $ zip uniqEls uniqIds
      idElemTable = reverseMap elemIdTable --DM.fromList $ zip uniqIds uniqEls
      idPairs     = [( fromJust $ DM.lookup e1 elemIdTable,
                       fromJust $ DM.lookup e2 elemIdTable) | (e1, e2) <- pairs]      
      comps = runST $ do
        quf  <- new (length uniqEls)
        quf' <- unionMany quf idPairs
        --return $ count quf'
        allNeighbors quf' uniqIds

      reverseLookUp keys = [fromJust $ DM.lookup k idElemTable | k <- keys]
  
    in map reverseLookUp comps


       
test1 = testQUF "./test-data/union-find/tinyUF.txt"
test2 = testQUF "./test-data/union-find/mediumUF.txt"
test3 = testQUF "./test-data/union-find/largeUF.txt"
  
-- test = do
--   open the test file
--   read the elements
--   do some stuff
  
