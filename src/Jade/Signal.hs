{-# LANGUAGE FlexibleContexts #-}
module Jade.Signal ( width
                   , getBundle
                   , explode
                   , getIndexesWithName
                   , hasSigName
                   ) where

import Jade.Common
import qualified Jade.Bundle as Bundle
import Control.Monad
import Data.Maybe

width :: Signal -> Maybe Int
width (Signal _ (Just w) _) = Just w
width (Signal (Just sigb) _ _) = Just $ Bundle.width sigb
width _ = Nothing

getIndexesWithName (Signal (Just bundle) _ _) name = Bundle.getIndexesWithName bundle name

getBundle (Signal (Just b) _ _) = b
getBundle (Signal Nothing _ _) = mempty

hasSigName (Signal (Just b) _ _) = Bundle.hasSigName b
hasSigName (Signal Nothing _ _) = False

explode (Signal (Just bundle) _ dir) =
  let subsignal x = Signal (Just $ Bundle [x]) (Just 1) dir
  in map subsignal (Bundle.getVals bundle)
explode s = [s]
