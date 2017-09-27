{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module Jade.Types where

import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBL8 
import Jade.Util
import Data.Hashable
import Text.Format
import Data.Aeson
import Rawr.Note
import Jade.Decode.Types

------------------------------------------------------------------
data Node = Node { nodeLocation :: (Integer, Integer)
                 , nodePart :: Part 
                 } deriving (Eq, Show, Generic, Hashable, Ord, ToJSON)

data Net = Net { netId :: NetId
               , netNodes :: [Node]
               } deriving (Generic, Show, Eq, Ord, ToJSON)

data Edge = Edge { edgeNode1 :: Node
                 , edgeNode2 :: Node
                 } deriving (Generic, Show, Hashable, Ord, Eq, ToJSON)

data QuickUnionUF a = QuickUnionUF { ids :: V.Vector Int
                                   , store :: DM.Map a Int
                                   , curId :: Int
                                   } deriving (Show)

