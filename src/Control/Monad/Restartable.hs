{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Monad.Restartable
  ( Persistent(..)
  , Restartable
  , step
  , runRestartable
  ) where

import Control.Applicative (liftA)
import Control.Monad       (ap)
import Data.Persistent
import System.IO           (Handle, IOMode(ReadWriteMode), withFile)

data Restartable a where
  Step :: Persistent a => String -> IO a -> Restartable a
  Pure :: a -> Restartable a
  Bind :: Restartable a -> (a -> Restartable b) -> Restartable b

instance Functor Restartable where
  fmap = liftA

instance Applicative Restartable where
  pure = Pure
  (<*>) = ap

instance Monad Restartable where
  (>>=) = Bind

step :: Persistent a => String -> IO a -> Restartable a
step = Step

runRestartable :: forall a . FilePath -> Restartable a -> IO a
runRestartable path restartable = withFile path ReadWriteMode run
  where
    run :: Handle -> IO a
    run handle = go restartable
      where
        go :: Restartable b -> IO b
        go = \case
          Step name act -> do
            maybeA <- restore handle
            case maybeA of
              Just a -> do
                putStrLn $ "step " <> name <> " already completed"
                pure a
              Nothing -> do
                putStrLn $ "running step " <> name
                a <- act
                save handle a
                pure a
          Pure a ->
            pure a
          Bind act f ->
            go act >>= (go . f)
