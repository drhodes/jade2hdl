{-# LANGUAGE FlexibleContexts #-}
module Jade.Signal where

import Jade.Types
import qualified Jade.Sig as Sig

import Control.Monad
import Data.Maybe

width :: Signal -> Maybe Integer
width (Signal _ (Just w) _) = Just w
width (Signal (Just sig) _ _) = Just $ Sig.width sig
width _ = Nothing


getName :: Signal -> J (Maybe String)
getName (Signal (Just sig) _ _) = liftM Just (Sig.getName sig)
getName _ = return Nothing
