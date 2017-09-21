module Jade.OO where

import Control.Monad.Except 
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

-- --         Exception handling.
-- --         |               Global state for memoization.
-- --         |               |      State type, global
-- --         |               |      |      Log handling.
-- --         |               |      |      |                return val.
-- --         |               |      |      |                |
-- type J a = ExceptT String (StateT Global (Writer [Note])) a 

data Point = Point { x :: Int
                   , y :: Int
                   }

--getX :: Self Point Int



--getX :: Monad m => m Point Int

infixl 9 ·

(·) x f = f <$> x

self = get

getX :: State Point Int
getX = self·x 
getY = self·y

mul = liftM2 (*)

distance = do self·x
              self·y

-- Food = do
--   p getX
  
