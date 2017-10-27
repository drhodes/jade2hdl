{-# LANGUAGE FlexibleContexts #-}

module Jade.Common
  ( module Jade.Common
  , module Jade.Types
  , module Rawr.Note
  , module Jade.Util
  , module Jade.Decode.Types
  ) where

import Jade.Types
import Rawr.Note
import Jade.Util
import Jade.Decode.Types
import qualified Data.Map as DM 
import qualified Data.Set as DS
import qualified Data.ByteString.Lazy.Char8 as DBL8 
import Data.Aeson
import Text.Format
import qualified Text.PrettyPrint.Leijen as P
import Data.Monoid
import Text.Printf

import Control.Monad.Except 
import Control.Monad.Writer
import Control.Monad.State

--         Exception handling.
--         |              Global state for memoization.
--         |              |      State type, global
--         |              |      |       Log handling.
--         |              |      |       |              return val.
--         |              |      |       |              |
type J a = ExceptT P.Doc (StateT Global (Writer P.Doc)) a 

data Global = Global { globalTopLevel :: TopLevel
                     , globalMemo :: Memo
                     }

data Memo = Memo { memoComps :: DM.Map String [DS.Set Part] }

instance Monoid P.Doc where
  mappend p q = p P.<> P.softline  P.<> q
  mempty = P.empty

emptyMemo = Memo DM.empty
emptyTopl stage = TopLevel stage DM.empty

getMemo :: J Memo
getMemo = globalMemo <$> get

putMemo memo = do
  Global x _ <- get
  put $ Global x memo

getTop :: J TopLevel
getTop = globalTopLevel <$> get
putTop top = do g <- get
                put g{globalTopLevel=top}
                
getCurStage = topLevelStage <$> getTop

todo msg = nb $ "todo: " ++ msg

globalInit topl = Global topl emptyMemo

runX :: TopLevel -> J a -> (Either P.Doc a, P.Doc)
runX topl x = let stateV = runExceptT x
                  writerV = evalStateT stateV (globalInit topl)
              in runWriter writerV

runLog :: TopLevel -> J a -> P.Doc
runLog topl x = snd $ runX topl x

runJ topl x = fst (runX topl x)

printJ topl x = case runJ topl x of
                  Left doc -> putStrLn $ show doc
                  Right val -> print val

putStrJ topl x = case runJ topl x of
                   Left doc -> putStrLn $ show doc
                   Right val -> putStr val

runJIO :: TopLevel -> J (IO a) -> IO P.Doc
runJIO topl x =
  case runX topl x of
    (Left doc, log) -> return $ doc P.<$$> log
    (Right f, log) -> f >> return log

--die msg = throwError ("! Oops" ++ "\n" ++ "! " ++ msg)
die msg = throwError $ P.text msg
dief msg xs = die $ format msg xs

silentBail :: J ()
silentBail = die ""


impossible msg = die $ "The impossible happened:" ++ msg

unimplemented :: J a
unimplemented = die "unimplemented."

cnb _ = return ()

nb :: String -> J ()
nb s = tell (P.text s)
nbf s xs = nb $ format s xs
nbd doc = tell doc
assert cond reason = unless cond (die reason)

enb x = tell (P.pretty x)
list xs = enb xs
listd xs = tell $ P.list $ map P.pretty xs


bail :: J a
bail = die "bailing!"
bailWhen cond = when cond bail

(?) x msg = let crash e = throwError $ e P.<+> (P.text ("\n" ++ "! " ++ msg))
            in x `catchError` crash

-- | for building execution traces.
(<?) msg f = do
  tell $ do P.empty P.<+> (P.nest 2 $ P.text msg)
  result <- f ? msg
  return result

safeCycle [] = "Jade/Common.safeCycle" <? die "empty list"
safeCycle xs = return $ cycle xs

------------------------------------------------------------------
-- RESOLVING CIRCUILAR IMPORTS :/

-- -- |Get a module from a TopLevel given a module name
-- getModule :: String -> J Module
-- getModule name = do -- "TopLevel.getModule" <? do  
--   if name `startsWith` "/gate"
--     then return $ BuiltInModule name
--     else do TopLevel stage m <- getTop
--             case DM.lookup name m of
--               Just mod -> return mod{moduleName = name}
--               Nothing -> die $ "TopLevel.getModule couldn't find module:" ++ name   

-- |Get a module from a TopLevel given a module name
getModule :: String -> J Module
getModule name = do -- "TopLevel.getModule" <? do  
  -- if name `startsWith` "/gate"
  -- then return $ BuiltInModule name
  TopLevel _ m <- getTop
  case DM.lookup name m of
    Just mod -> return mod{moduleName = name}
    Nothing -> die $ "TopLevel.getModule couldn't find module:" ++ name

getSchematic :: String -> J Schematic
getSchematic name = do
  m <- getModule name
  case moduleSchem m of
    Just s -> return s
    Nothing -> die $ printf "(No schematic found in module: <%s>)" name --ok

assertEq :: (Eq a, Show a) => a -> a -> String -> J ()
assertEq x y msg = when (x /= y) (die $ printf "Assertion failed: %s, %s" (show msg) (show (x, y))) --ok

assertStage expstage = do
  s <- getCurStage
  let msg = printf "In wrong stage, expected: %s, got: %s" (show expstage) (show s) --ok
  assertEq expstage s msg
