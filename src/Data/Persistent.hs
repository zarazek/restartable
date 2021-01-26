{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Persistent
  ( Persistent(..)
  , save
  , restore
  ) where

import System.IO (Handle, hIsEOF, hPutStrLn, hGetLine)

class Persistent a where
  serialize :: a -> String
  default serialize :: Show a => a -> String
  serialize = show
  
  deserialize :: String -> a
  default deserialize :: Read a => String -> a
  deserialize = read

instance Persistent ()
instance Persistent Bool
instance Persistent String

save :: Persistent a => Handle -> a -> IO ()
save handle a = hPutStrLn handle $ serialize a

restore :: Persistent a => Handle -> IO (Maybe a)
restore handle = do
  eof <- hIsEOF handle
  if eof then
    pure Nothing
  else
    Just . deserialize <$> hGetLine handle
