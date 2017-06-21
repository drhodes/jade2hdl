{-# LANGUAGE OverloadedStrings #-}
module Jade.Graph where
  -- ( Graph(..)
  -- , Edge(..)
  -- , addEdge
  -- , adjacent
  -- , collect
  -- , components
  -- , degree
  -- , empty
  -- , fromEdges
  -- , numVerts
  -- ) where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL
import qualified Data.Hashable as DH
import qualified Web.Hashids as WH
import Jade.Types

empty = Graph DM.empty

addEdge' (Graph table) (Edge v w) =
  let table' = case DM.lookup v table of
                 Nothing -> DM.insert v (DS.fromList [w]) table
                 Just s -> DM.insert v (DS.insert w s) table
  in Graph table'

addEdge g (Edge v w) =
  let g1 = addEdge' g (Edge v w)
      g2 = addEdge' g1 (Edge w v)
  in g2

adjacent (Graph table) v =
  case DM.lookup v table of
    Just s -> s
    Nothing -> DS.empty

degree g v = DS.size $ adjacent g v   
numVerts (Graph table) = DM.size table
verts (Graph table) = DM.keys table
        
fromEdges xs = foldl addEdge empty xs

-- breadth first search.
--collect' :: Ord a => Graph (Node a) -> DS.Set (Node a) -> a -> DS.Set (Node a)
collect' g seen vert =
  let adj = adjacent g vert -- get the adjacent nodes
      seen' = DS.union adj seen -- union them with those seen already
      unexplored = adj DS.\\ seen 
      next = DS.unions $ map (collect' g seen') (DS.toList unexplored)
  -- in DS.union next seen
  in DS.union seen (DS.union next seen')

collect g vert = DS.union (DS.fromList [vert]) (collect' g DS.empty vert)

components :: Ord t => Graph t -> [DS.Set (Node t)]
components g = DS.toList $ DS.map (collect g) (DS.fromList $ verts g)

testG = fromEdges [ Edge (Node 2 Nop) (Node 4 Nop)
                  -- , Edge 4 6
                  -- , Edge 6 8
                  -- , Edge 1 3
                  -- , Edge 3 5
                  -- , Edge 5 7
                  ]



hashComp :: GComp -> String
hashComp gcomp =
  let ctx = WH.hashidsSimple "salt"
      in show $ WH.encode ctx $ DH.hash $ DS.toList gcomp
