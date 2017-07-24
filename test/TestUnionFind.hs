module TestUnionFind where


import Jade.UnionFindST
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.STRef
import Jade.Types hiding (ids)
import qualified Data.List as DL
import qualified Data.Map as DM



testComponents = do
  let n1 = Node (0,0) (WireC (Wire (Coord5 0 0 Rot0 0 0) (Just (Signal (Just (SigSimple "in1")) Nothing Nothing))))
      n2 = Node (0,0) (TermC (Terminal (Coord3 0 0 Rot0) (SigSimple "A")))
      --n3 = Node (0,0) (PortC (Port (Coord3 0 0 Rot0) (Just (Signal (Just (SigSimple "in1")) Nothing Nothing))))


  
  let edges1 = [Edge n1 n2]
      edges2 = [Edge n2 n1]
      
      comps1 = components edges1
      comps2 = components edges2
      
  print comps1 -- == comps2
  print comps2




main = runST $ do
    uf <- newUnionFind 10
    unite uf 3 4 -- 0, 1, 2, {3, 4}, 5, 6, 7, 8, 9
    unite uf 4 9 -- 0, 1, 2, {3, 4, 9}, 5, 6, 7, 8
    unite uf 8 0 -- {0, 8}, 1, 2, {3, 4, 9}, 5, 6, 7, 8
    unite uf 2 3 -- {0, 8}, 1, {2, 3, 4, 9}, 5, 6, 7
    unite uf 5 6 -- {0, 8}, 1, {2, 3, 4, 9}, {5, 6}, 7
    unite uf 5 9 -- {0, 8}, 1, {2, 3, 4, 5, 6, 9}, 7
    unite uf 7 3 -- {0, 8}, 1, {2, 3, 4, 5, 6, 7, 9}
    unite uf 4 8 -- 1, {0, 2, 3, 4, 5, 6, 7, 8, 9}
    -- find uf 1 2 -- False
    xs <- sequence [filterM (find uf x) [0..9] | x <- [0..9]]
    return $ DL.nub xs
