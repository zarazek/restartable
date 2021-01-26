{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Monad.Restartable
  ( Restartable
  , step
  , runRestartable
  ) where

import Control.Monad   (liftM, ap, (>=>))
import Data.Persistent
import System.IO       (Handle, IOMode(ReadWriteMode), withFile)

data Freer f a where
  Pure   :: a -> Freer f a
  Impure :: f x -> (x -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap = liftM

instance Applicative (Freer f) where
  pure = Pure
  (<*>) = ap

instance Monad (Freer f) where
  (Pure a)      >>= g = g a
  (Impure fx f) >>= g = Impure fx (f >=> g)

liftF :: f a -> Freer f a
liftF command = Impure command Pure

interpret :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
interpret nt = go
  where
    go = \case
      Pure a         -> pure a
      Impure fx cont -> nt fx >>= (go . cont)

data RestartableF a where
  Step :: Persistent a => String -> IO a -> RestartableF a

type Restartable = Freer RestartableF

step :: Persistent a => String -> IO a -> Restartable a
step name act = liftF $ Step name act

runRestartable :: forall a . FilePath -> Restartable a -> IO a
runRestartable path restartable = withFile path ReadWriteMode run
  where
    run :: Handle -> IO a
    run handle = interpret go restartable
      where
        go :: RestartableF b -> IO b
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
