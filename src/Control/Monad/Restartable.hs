{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Monad.Restartable
  ( Restartable
  , step
  , runRestartable
  ) where

import Control.Monad   (liftM, ap)
import Data.Persistent
import System.IO       (Handle, IOMode(ReadWriteMode), withFile)

data Free f a = Pure a
              | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap = liftM

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (Pure a) >>= f = f a
  (Impure i) >>= f = Impure ((>>= f) <$> i)

liftF :: Functor f => f a -> Free f a
liftF command = Impure (Pure <$> command)

interpret :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
interpret nt = go
  where
    go = \case
      Pure a    -> pure a
      Impure fx -> nt fx >>= go

data RestartableF next where
  Step :: Persistent a => String -> IO a -> (a -> next) -> RestartableF next

instance Functor RestartableF where
  fmap g (Step name act f) = Step name act (g . f)

type Restartable = Free RestartableF

step :: Persistent a => String -> IO a -> Restartable a
step name act = liftF $ Step name act id

runRestartable :: forall a . FilePath -> Restartable a -> IO a
runRestartable path restartable = withFile path ReadWriteMode run
  where
    run :: Handle -> IO a
    run handle = interpret go restartable
      where
        go :: RestartableF b -> IO b
        go = \case
          Step name act cont -> do
            maybeA <- restore handle
            case maybeA of
              Just a -> do
                putStrLn $ "step " <> name <> " already completed"
                pure $ cont a
              Nothing -> do
                putStrLn $ "running step " <> name
                a <- act
                save handle a
                pure $ cont a
