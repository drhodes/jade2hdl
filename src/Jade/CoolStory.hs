{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text as T

import Control.Monad.Except 
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

data ErrType = Debug
             | Warning
             | Fatal
             | Impossible
               deriving (Show, Generic, ToJSON, FromJSON)

data Err = Err { errLog :: Log
               , errType :: ErrType
               } deriving (Show, Generic, ToJSON, FromJSON)

data Log = Log (V.Vector T.Text) deriving (Show, Generic, ToJSON, FromJSON)

-- safeDiv :: Integer -> Integer -> ExceptT Err (Writer Log) Integer
--type StoryT m a = ExceptT Err (WriterT Log m) a
newtype StoryT m a = StoryT { runStoryT  :: m (Story a) }

type Story a = ExceptT Err (Writer Log) a

instance Monoid Log where
  mappend (Log xs) (Log ys) = Log (xs <> ys)
  mempty = Log V.empty

asdf = decode (DBL8.pack "{\"errType\":\"Fatal\",\"errString\":\"denom can't be 0!\"}") :: Maybe Err
nb s = tell (Log $ V.fromList [s])

safeDiv :: Integer -> Integer -> Story Integer
safeDiv numer denom = do
  nb "Checking for divide by zero"
  
  when (denom == 0) $ fatal "denom can't be 0!"
  return $ numer `div` denom

performSafeDiv = safeDiv 9 0

step1 :: Writer Log (Either Err Integer)
step1 = runExceptT performSafeDiv

step2 :: (Either Err Integer, Log)
step2 = runWriter step1

runStory :: Story a -> Either Err a
runStory f = 
  let writer = runExceptT f 
  in case runWriter writer of
    (Right x, log) -> Right x
    (Left stackTrace, log) -> Left stackTrace -- log

jsonStory :: Story a -> Either DBL8.ByteString a
jsonStory f =
  case runStory f of
    Right x -> Right x
    Left errlog -> Left $ encode errlog


debug s = encode $ Err s Debug
fatal s = throwError $ Err (Log (V.fromList [T.pack s])) Fatal

die s = fatal s
dief msg xs = die $ format msg xs

--wut msg = throwError $ Err (T.pack $ "The impossible happened: " ++ msg) Impossible

bail :: Story ()
bail = die "Bailing!"

(?) :: Story a -> Err -> Story a
(?) x msg = let crash e = throwError e -- <> msg)
            in x `catchError` crash



{-

impossible msg = die $ "The impossible happened: " ++ msg

-- unimplemented = die "unimplemented."

nb s = tell [s]
nbf s xs = nb $ format s xs

list x = do nb $ DL.intercalate "\n" $ map show x
            nb ""

-- bail = die "bailing!"
-- bailWhen cond = when cond bail


-- | for building nice stack traces.
(<?) msg x = nb msg >> x ? msg


-}
