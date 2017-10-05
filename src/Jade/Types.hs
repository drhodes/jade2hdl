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
import Jade.Decode.Pretty
import Text.Printf
import Text.PrettyPrint.Leijen 

newtype Bundle a = Bundle [a] deriving (Show, Eq, Generic, Hashable, Ord, Foldable, ToJSON)

type NetId = Integer

data Val = ValIndex { valIdxName :: String
                    , valIdxIdx :: Index
                    }
         | NetIndex { netIdxId :: NetId
                    , netIdxIdx :: Index
                    }
         | Lit { litBinVal :: BinVal }
         deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

instance Pretty Val where
  pretty (ValIndex name idx) = text (printf "ValIndex name:%s, idx:%d" name idx)
  pretty (NetIndex name idx) = text (printf "NetIndex name:%s, idx:%d" name idx)
  pretty (Lit binval) = text "Lit" <+> pretty binval

type ValBundle = Bundle Val

instance Functor Bundle where
  fmap f (Bundle xs) = Bundle (map f xs) 
instance Applicative Bundle where
  pure x = Bundle [x]
  (<*>) (Bundle fs) (Bundle xs) = Bundle (fs <*> xs)
instance Monad Bundle where
  (>>=) (Bundle xs) f = Bundle $ concat [ys | Bundle ys <- map f xs]
instance Monoid (Bundle a) where
  mconcat bs = Bundle $ concat [x | Bundle x <- bs]
  mappend (Bundle x) (Bundle y) = Bundle (x ++ y)
  mempty = Bundle []


------------------------------------------------------------------
-- introducing another stage for inferring wire size 

data PostSignal = PostSignal { postSignalBundle :: Maybe ValBundle
                             , postSignalWidth :: Int
                             , postSignalDirection :: Maybe Direction
                             } deriving (Show, Eq, Generic, Hashable, Ord, ToJSON)

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

-- data QuickUnionUF a = QuickUnionUF { ids :: V.Vector Int
--                                    , store :: DM.Map a Int
--                                    , curId :: Int
--                                    } deriving (Show)

