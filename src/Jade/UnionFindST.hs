module Jade.UnionFindST (components) where

-- this code swiped from https://gist.github.com/kseo/8693028

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.STRef
import Jade.Types hiding (ids)
import qualified Data.List as DL
import qualified Data.Map as DM
import Prelude hiding (id)

data UnionFind s = UnionFind { ids :: STUArray s Int Int
                             , szs :: STUArray s Int Int }


newUnionFind :: Int
             -- ^ specify the max number of elements.
             -> ST s (UnionFind s)
newUnionFind n = liftM2 UnionFind (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)

find :: UnionFind s
     -- ^ a union find object
     -> Int
     -- ^ an index into the id array
     -> Int
     -- ^ an index into the id array
     -> ST s Bool
find uf p q = liftM2 (==) (root uf p) (root uf q)

root :: UnionFind s -> Int -> ST s Int
root uf i = do
    id <- readArray (ids uf) i
    if id /= i
        then do
            gpid <- readArray (ids uf) id
            writeArray (ids uf) i gpid
            root uf id
        else return i

unite :: UnionFind s -> Int -> Int -> ST s ()
unite uf p q = do
    i <- root uf p
    j <- root uf q
    szi <- readArray (szs uf) i
    szj <- readArray (szs uf) j
    if szi < szj
      then do writeArray (ids uf) i j
              writeArray (szs uf) j (szi + szj)
      else do writeArray (ids uf) j i
              writeArray (szs uf) i (szj + szi)
-- end of swipe from
-- https://gist.github.com/kseo/8693028

components :: [Edge] -> [Net]
components edges = runST $ do
  let nodes = DL.nub $ DL.sort $ concat [[n1, n2] | Edge n1 n2 <- edges, n1 /= n2]
      table = DM.fromList $ zip nodes [0..]
  uf <- newUnionFind (length nodes)

  let tie (Edge n1 n2) =
        case (DM.lookup n1 table, DM.lookup n2 table) of
          (Just idx1, Just idx2) -> unite uf idx1 idx2
          _ -> error "Can't find somehitkas asdf asdfkasjdf"

  mapM_ tie edges
  
  xs <- sequence [filterM (find uf x) [0.. length nodes - 1] | x <- [0.. length nodes - 1]]
  let indexes = DL.nub xs
  return $ zipWith Net [1..] [DL.nub $ map (nodes !!) xs | xs <- indexes]

