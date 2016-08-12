module Hashed (
  Hashed
 ,unhash
 ,toHashed
) where

import Data.ByteString.Lazy (ByteString)
import Data.Digest.Pure.MD5
import Data.Maybe
import Control.Monad.Free

verifyHash :: String -> ByteString -> Maybe ByteString
verifyHash h s
  | show (hash s :: MD5Digest) == h = Just s
  | otherwise   = Nothing

data Verified m next = H (m ByteString) String (ByteString -> next)

instance Functor (Verified m) where
  fmap f (H s h x) = H s h (f.x)

type Hashed m = Free (Verified m)

unhash :: (Monad m) => Hashed m a -> m (Maybe a)
unhash (Pure a) = pure (Just a)
unhash (Free (H s h f)) = fmap (verifyHash h) s >>= maybe (pure Nothing) (unhash . f)

toHashed :: m ByteString -> String -> Hashed m ByteString
toHashed s h = Free (H s h Pure)
