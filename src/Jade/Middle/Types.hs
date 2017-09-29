{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module Jade.Middle.Types where

import GHC.Generics
import Data.Aeson
import Control.Monad
import Jade.Common
import Text.Format
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Vector as V
import qualified Jade.Decode.Decode as Decode
import qualified Jade.Net as Net
import qualified Jade.Module as Module
import qualified Jade.MemUnit as MemUnit
import qualified Jade.Decode.Val as Val
import qualified Jade.TopLevel as TopLevel
import qualified Jade.Decode.Bundle as Bundle

data TermAssoc = TermAssoc { taDir :: Direction
                           , taSrc :: Val
                           , taTgt :: Val
                           } deriving (Generic, ToJSON, Show, Eq)

data ValAssign = ValAssign { sigAssignSrc :: Val
                           , sigAssignTgt :: Val
                           } deriving (Generic, ToJSON, Show, Eq)


flipDir In = Out
flipDir Out = In

flipTermAssoc (TermAssoc dir src tgt) = TermAssoc (flipDir dir) tgt src
flipTermMap xs = map flipTermAssoc xs

type TermMap = [TermAssoc]

termAssocTgtFromSrcName (TermAssoc _ src tgt) name = if Val.hasIdent src name
                                                     then Just tgt
                                                     else Nothing

-- replicated submodule.
data SubModuleRep = SubModuleRep { smrTermMapInput :: [TermMap]
                                   -- ^ all inputs for this slice of the 
                                 , smrTermMapOutput :: [TermMap]
                                 , smrSubModule :: SubModule
                                 , smrZIndex :: Int
                                 } deriving (Generic, ToJSON, Show, Eq)

data ModOutput = ModOutput TermMap deriving (Generic, ToJSON, Show, Eq)

