{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Jade.UnionFind
  ( components
  , fromEdges
  , nameComp
  ) where

import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import Jade.Types
------------------------------------------------------------------

new :: Int -> QuickUnionUF a
new n = QuickUnionUF (V.fromList [0..n]) DM.empty 0

root :: QuickUnionUF a -> Int -> Int
root q@(QuickUnionUF vec store _) idx =
  let i = vec V.! idx
  in if i == idx
     then i
     else root q i

connected' :: QuickUnionUF a -> Int -> Int -> Bool
connected' quf p q = (root quf p) == (root quf q)

union' :: QuickUnionUF a -> Int -> Int -> QuickUnionUF a
union' quf@(QuickUnionUF vec _ _) p q = 
    let i = root quf p
        j = root quf q
    in quf{ids = vec V.// [(i, j)]}

addVertex :: Ord a => QuickUnionUF a -> a -> QuickUnionUF a
addVertex quf@(QuickUnionUF ids store curId) vert =
  case DM.lookup vert store of
    Just _ -> quf -- already has this vert, so just return quf.
    _ -> QuickUnionUF ids (DM.insert vert curId store) (curId + 1)

addVertexes :: Ord a => QuickUnionUF a -> [a] -> QuickUnionUF a
addVertexes quf vs = foldl addVertex quf vs

connected quf v1 v2 =
  let el1 = DM.lookup v1 (store quf) 
      el2 = DM.lookup v2 (store quf) 
  in case (el1, el2) of
    (Just p, Just q) -> connected' quf p q
    _ -> error "Couldn't find el1 or el2 in union find"

union quf v1 v2 =
  let el1 = DM.lookup v1 (store quf)
      el2 = DM.lookup v2 (store quf)
  in case (el1, el2) of
    (Just p, Just q) -> union' quf p q
    _ -> error "Couldn't find el1 or el2 in union find"

connectedTo :: Ord a => QuickUnionUF a -> a -> [a]
connectedTo quf v = 
  let vs = DM.keys (store quf)
  in DL.nub $ DL.sort $ filter (connected quf v) vs

components :: Ord a => QuickUnionUF a -> [[a]]
components quf = let ks = DM.keys (store quf)
                 in DL.nub $ map (connectedTo quf) ks

nodesFromEdge (Edge n1 n2) = [n1, n2]

addEdge quf (Edge n1 n2) = union quf n1 n2
addEdges quf [] = quf
addEdges quf (e:rest) = addEdges (addEdge quf e) rest

fromEdges :: Ord t => [Edge t] -> QuickUnionUF (Node t)
fromEdges es =
  let nodes = concat $ map nodesFromEdge es
      set = DS.fromList nodes
      quf = new (DS.size set)
      quf' = addVertexes quf nodes
  in addEdges quf' es

nameComp :: [Node a] -> J String
nameComp nodes = "UnionFind.nameComp" <? do
  let parts = map nodePart nodes
      signals = [signal | WireC (Wire _ (Just signal)) <- parts]
      names = [n | Signal (Just (SigSimple n)) _ _ <- signals]
      genNameLen = 10
  
  return $ if length names > 0
           then head names
           else take genNameLen $ "wire_" ++ hashid parts
  
test1 = do
  let q = new 10
      q1 = addVertex q 'a'
      q2 = addVertex q1 'b'
      q3 = addVertex q2 'c'
      q4 = union q3 'a' 'b'

  print $ connectedTo q4 'a' 
