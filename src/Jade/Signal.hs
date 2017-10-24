{-# LANGUAGE FlexibleContexts #-}
module Jade.Signal where

import Jade.Common
--import qualified Jade.Bundle as Bundle
import qualified Jade.Decode.Sig as Sig
import Control.Monad
import Data.Maybe

width :: Signal -> Int
-- TODO change Sig.width to return Int
width (Signal (Just sig) _ _) = fromIntegral $ Sig.width sig
width (Signal _ w _) = w

getSig :: Signal -> Maybe Sig
getSig (Signal ms _ _) = ms

-- hasSigName (Signal (Just b) _ _) = Bundle.hasSigName b
-- hasSigName (Signal Nothing _ _) = False


{-
getIndexesWithName (Signal (Just bundle) _ _) name = Bundle.getIndexesWithName bundle name
-}

{-
getBundle (Signal (Just b) _ _) = b
getBundle (Signal Nothing _ _) = mempty
-}

{-
-}

{-
explode (Signal (Just bundle) _ dir) =
  let subsignal x = Signal (Just $ Bundle [x]) 1 dir
  in map subsignal (Bundle.getVals bundle)
explode s = [s]
-}
