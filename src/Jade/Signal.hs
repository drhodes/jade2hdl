{-# LANGUAGE FlexibleContexts #-}
module Jade.Signal where

import Jade.Types
import qualified Jade.Bundle as Bundle
import Jade.Util
import Control.Monad
import Data.Maybe

width :: Signal -> Maybe Int
width (Signal _ (Just w) _) = Just w
width (Signal (Just sigb) _ _) = Just $ Bundle.width sigb
width _ = Nothing

getNames :: Signal -> [String]
getNames (Signal (Just bundle) _ _) = Bundle.getNames bundle
getNames _ = return []


getBundle (Signal (Just b) _ _) = b
getBundle (Signal Nothing _ _) = mempty
