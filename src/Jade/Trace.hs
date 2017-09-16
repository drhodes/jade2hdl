{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}

module Jade.Story where

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

import Control.Monad.Except 
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Applicative
import Data.Char

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } deriving (Functor, Applicative)

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just

  x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                        case maybe_value of
                          Nothing -> return Nothing
                          Just val -> runMaybeT $ f val


instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                        case maybe_value of
                          Nothing -> runMaybeT y
                          Just _ -> return maybe_value


instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans MaybeT where
  lift = MaybeT . (liftM Just)





isValid s = and [ length s > 8
                , any isAlpha s
                , any isNumber s
                ]

getPassphrase :: MaybeT IO String
getPassphrase = do
  password <- lift getLine
  guard (isValid password)
  return password

askPassphrase :: MaybeT IO ()
askPassphrase = do
  lift $ putStrLn "enter passphrase"
  pass <- getPassphrase
  lift $ putStrLn "storing in database"

----------------------------------------------------------------------------------------

data Trace a = Trace [a] deriving (Show, Eq)

instance Functor Trace where
  fmap f (Trace xs) = Trace $ fmap f xs

instance Applicative Trace where
  pure x = Trace [x]
  (<*>) (Trace fs) (Trace xs) = Trace [f x | (f, x) <- zip fs xs]

instance Monad Trace where
  (>>=) (Trace xs) f = Trace $ concat [x | Trace x <- map f xs ]

newtype TraceT m a = TraceT { runTraceT :: m (Trace a) } deriving (Functor, Applicative)



instance Monad m => Monad (TraceT m) where
  return x = TraceT $ return $ Trace [x]

  x >>= f = TraceT $
            do Trace xs <- runTraceT x
               --Trace ys <- runTraceT (f x)
               undefined
  
  --return = MaybeT . return . Just

  -- x >>= f = MaybeT $ do maybe_value <- runMaybeT x
  --                       case maybe_value of
  --                         Nothing -> return Nothing
  --                         Just val -> runMaybeT $ f val
