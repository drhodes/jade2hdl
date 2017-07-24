module Jade.UnionFindST where

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
                             , szs :: STUArray s Int Int
                             }

newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftM2 UnionFind (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)

find :: (UnionFind s) -> Int -> Int -> ST s Bool
find uf p q = liftM2 (==) (root uf p) (root uf q)

root :: (UnionFind s) -> Int -> ST s Int
root uf i = do
    id <- readArray (ids uf) i
    if (id /= i)
        then do
            gpid <- readArray (ids uf) id
            writeArray (ids uf) i gpid
            root uf id
        else return i

unite :: (UnionFind s) -> Int -> Int -> ST s ()
unite uf p q = do
    i <- root uf p
    j <- root uf q
    szi <- readArray (szs uf) i
    szj <- readArray (szs uf) j
    if (szi < szj)
        then do
            writeArray (ids uf) i j
            writeArray (szs uf) j (szi + szj)
        else do
            writeArray (ids uf) j i
            writeArray (szs uf) i (szj + szi)
            
-- end of swipe from
-- https://gist.github.com/kseo/8693028

  
-- components :: _
-- components = undefined

--fromEdges :: Ord t => [Edge t] -> UnionFind (Node t)

--fromEdges :: Foldable t => t a -> ST s (UnionFind s)

-- https://hackage.haskell.org/package/extra-1.6/docs/src/Control-Monad-Extra.html#partitionM
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)


collect :: UnionFind s -> [Int] -> ST s [[Int]]
collect _ [] = return []
collect uf (x:rest) = do
  (cnx, notcnx) <- partitionM (find uf x) rest
  liftM (cnx:) (collect uf notcnx)


--components :: [Edge] -> [GComp]
--components [] = []
-- components edges = runST $ do
--   let nodes = DL.nub $ concat [[n1, n2] | (Edge n1 n2) <- edges]
--       table = DM.fromList (zip nodes [0..])

--   uf <- newUnionFind $ length nodes
        
--   let f (Edge node2 node1) =
--         case (DM.lookup node1 table, DM.lookup node2 table) of
--           (Just i, Just j) -> unite uf j i
--           _ -> return ()
--   mapM_ f edges

--   -- This is just completely insane.  I've some how screwed up the
--   -- collect function here, need to really take a hard look at it.
--   xss <- collect uf $ reverse $ DM.elems table --[0 .. length nodes - 1]
--   return $ map (helper nodes) xss

--getConnected :: UnionFind s -> [Int] -> [Int] -> ST s [[Int]]

-- getConnected :: UnionFind s -> [Int] -> [Int] -> ST s [[Int]]
-- getConnected _ _ [] = return []
-- getConnected uf alreadySeen (nodeIdx:rest) = do
--   if nodeIdx `elem` alreadySeen
--     then getConnected uf alreadySeen rest
--     else do connectedNodes <- filterM (find uf nodeIdx) rest
--             liftM (connectedNodes:) (getConnected uf (alreadySeen ++ connectedNodes) rest)

--components :: [Edge] -> [GComp]
components edges = runST $ do
  let nodes = DL.nub $ DL.sort $ concat [[n1, n2] | Edge n1 n2 <- edges]
      table = DM.fromList $ zip nodes [0..]
      elbat = DM.fromList $ zip [0..] nodes
  uf <- newUnionFind 100000

  let tie (Edge n1 n2) =
        case (DM.lookup n1 table, DM.lookup n2 table) of
          (Just idx1, Just idx2) -> unite uf idx1 idx2
          _ -> error "Can't find somehitkas asdf asdfkasjdf"

  mapM_ tie edges
  
  xs <- sequence [filterM (find uf x) [0.. length nodes - 1] | x <- [0.. length nodes - 1]]
  let results = DL.nub xs
  return $ [GComp $ DL.nub $ map (nodes !!) xs | xs <- results]


  --sequence [find uf x y | x <- DM.elems table, y <- DM.elems table]




helper :: [Node] -> [Int] -> GComp
helper nodes xs = GComp $ map (nodes !!) xs

  
nameComp :: GComp -> J String
nameComp (GComp nodes) = "UnionFind.nameComp" <? do
  let parts = map nodePart nodes
      signals1 = [signal | WireC (Wire _ (Just signal)) <- parts]
      --signals2 = [signal | PortC (Port _ (Just signal)) <- parts]
      names = [n | Signal (Just (SigSimple n)) _ _ <- signals1] -- ++ signals2]
      genNameLen = 10
  
  return $ if length names > 0
           then head names
           else take genNameLen $ "wire_" ++ hashid parts
