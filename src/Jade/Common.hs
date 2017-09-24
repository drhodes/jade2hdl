{-# LANGUAGE FlexibleContexts #-}
module Jade.Common
  ( module Jade.Common
  , module Jade.Types
  , module Jade.Note
  , module Jade.Util
  ) where

import Jade.Types
import Jade.Note
import Jade.Util

import qualified Data.Map as DM
--import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Map as DM
import qualified Data.List as DL
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Lazy.Char8 as DBL8 
import Data.Aeson
import Data.Hashable
import Text.Format
import Jade.Note



import Control.Monad.Except 
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

--         Exception handling.
--         |               Global state for memoization.
--         |               |      State type, global
--         |               |      |      Log handling.
--         |               |      |      |                return val.
--         |               |      |      |                |
type J a = ExceptT String (StateT Global (Writer [Note])) a 

data Global = Global { globalTopLevel :: TopLevel
                     , globalMemo :: Memo
                     }

data Memo = Memo { memoComps :: DM.Map String [Net] }

emptyMemo = Memo DM.empty
emptyTopl = TopLevel DM.empty

getMemo :: J Memo
getMemo = globalMemo <$> get

putMemo memo = do
  Global x _ <- get
  put $ Global x memo

getTop :: J TopLevel
getTop = globalTopLevel <$> get

globalInit topl = Global topl emptyMemo

runX :: TopLevel -> J a -> (Either String a, [Note])
runX topl x = let stateV = runExceptT x
                  writerV = evalStateT stateV (globalInit topl)
              in runWriter writerV

runLog :: TopLevel -> J a -> String
runLog topl x = let log = snd $ runX topl x
                in notesToString log

runCallGraph :: TopLevel -> J a -> String
runCallGraph topl x = let log = snd $ runX topl x
                      in dotGraph log

runJ topl x = fst (runX topl x)

printJ topl x = case runJ topl x of
                  Left msg -> putStrLn msg
                  Right val -> print val

putStrJ topl x = case runJ topl x of
                   Left msg -> putStrLn msg
                   Right val -> putStr val

runJIO :: TopLevel -> J (IO a) -> IO String
runJIO topl x =
  case runX topl x of
    (Left msg, log) -> return $ DBL8.unpack $ encode (uniq log ++ [Note msg])
    (Right f, log) -> f >> (return $ DBL8.unpack $ encode (uniq log))

die msg = throwError ("! Oops" ++ "\n" ++ "! " ++ msg)
dief msg xs = die (format msg xs)

impossible msg = die $ "The impossible happened: " ++ msg

unimplemented :: J a
unimplemented = die "unimplemented."

cnb _ = return ()

nb :: String -> J ()
nb s = tell [Note $ DBL8.unpack $ encode s]
nbf s xs = nb $ format s xs


assert cond reason = unless cond (die reason)

enb :: ToJSON a => a -> J ()
enb x = tell [Note $ DBL8.unpack $ encode x]
list xs = enb xs

bail :: J a
bail = die "bailing!"
bailWhen cond = when cond bail

(?) x msg = let crash e = throwError $ e ++ "\n" ++ "! " ++ msg
            in x `catchError` crash

-- | for building execution traces.
(<?) msg f = do
  tell [Func msg]
  result <- f ? msg
  tell [EndFunc]
  return result

safeCycle [] = "Jade/Types.safeCycle" <? die "empty list"
safeCycle xs = return $ cycle xs





------------------------------------------------------------------
-- RESOLVING CIRCUILAR IMPORTS :/


-- |Get a module from a TopLevel given a module name
getModule :: String -> J Module
getModule name = do -- "TopLevel.getModule" <? do  
  -- if name `startsWith` "/gate"
  -- then return $ BuiltInModule name
  TopLevel m <- getTop
  case DM.lookup name m of
    Just mod -> return mod{moduleName = name}
    Nothing -> die $ "TopLevel.getModule couldn't find module:" ++ name   

getSchematic name = do
  m <- getModule name
  case moduleSchem m of
    Just s -> return s
    Nothing -> dief "No schematic found in module" [name]


