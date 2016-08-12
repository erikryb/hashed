module Hashed (
  Hashed
 ,unhash
 ,toHashed
) where

import Data.ByteString.Lazy (ByteString)
import Data.Digest.Pure.MD5
import Data.Maybe

verifyHash :: String -> ByteString -> Maybe ByteString
verifyHash h s
  | show (hash s :: MD5Digest) == h = Just s
  | otherwise   = Nothing

data Hashed m a = HPure a | H (m ByteString) String (ByteString -> Hashed m a)

instance Functor (Hashed m) where
  fmap f (HPure a) = HPure (f a)
  fmap f (H s h g) = H s h (fmap f . g)

instance Applicative (Hashed m) where
  pure = HPure
  (<*>) (HPure f) x = fmap f x
  (<*>) (H s h f) x = H s h ((<*> x) . f)

instance Monad (Hashed m) where
  (>>=) (HPure x) f = f x
  (>>=) (H s h g) f = H s h ((>>= f) . g)

unhash :: (Monad m) => Hashed m a -> m (Maybe a)
unhash (HPure a) = pure (Just a)
unhash (H s h f) = fmap (verifyHash h) s >>= maybe (pure Nothing) (unhash . f)

toHashed :: m ByteString -> String -> Hashed m ByteString
toHashed s h = H s h HPure
