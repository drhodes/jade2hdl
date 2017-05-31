module Jade.Graph
  ( Graph(..)
  , Edge(..)
  , addEdge
  , adjacent
  , collect
  , components
  , degree
  , empty
  , fromEdges
  , numVerts
  ) where

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.List as DL

data Graph a = Graph (DM.Map a (DS.Set a)) deriving (Show, Eq)
data Edge a = Edge a a deriving (Show, Eq)

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
        
fromEdges :: Ord a => [Edge a] -> Graph a
fromEdges = foldl addEdge empty

collect' g seen vert =
  let adj = adjacent g vert -- Set
      seen' = DS.union adj seen
      keepers = adj DS.\\ seen
      next = DS.unions $ map (collect' g seen') (DS.toList keepers)
  in DS.union next seen
  
collect g vert = collect' g DS.empty vert  

components g = DS.map (collect g) (DS.fromList $ verts g)


testG = fromEdges [ Edge 2 4
                  , Edge 4 6
                  , Edge 6 8
                  , Edge 1 3
                  , Edge 3 5
                  , Edge 5 7
                  ]
