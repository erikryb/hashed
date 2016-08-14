{-# LANGUAGE DeriveFunctor #-}

module Hashed (
  Deterministic
 ,runMD5
 ,runNoCheck
 ,liftD
) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.MD5
import Data.Maybe
import Control.Monad.Free
import Control.Monad
import Data.Functor.Identity

md5hash :: ByteString -> String
md5hash s = show (hash s :: MD5Digest)

verifyMD5 :: String -> ByteString -> Either String ByteString
verifyMD5 h s
  | md5hash s == h = Right s
  | otherwise   = Left h

data Verified a h next = V h (a -> next) deriving (Functor)

type Deterministic a h = Free (Verified a h)

run :: (Traversable t, Monad t, Monad m) => (h -> a -> t a) -> Deterministic a (m a, h) b -> m (t b)
run v (Pure a) = pure (pure a)
run v (Free (V (a,h) f)) = fmap join (fmap (v h) a >>= sequence . fmap (run v . f))

runMD5 :: (Monad m) => Deterministic ByteString (m ByteString, String) b -> m (Either String b)
runMD5 = run verifyMD5

runNoCheck :: (Monad m) => Deterministic a (m a, h) b -> m b
runNoCheck = fmap runIdentity . run (const pure)

liftD :: m a -> h -> Deterministic a (m a, h) a
liftD a h = Free (V (a,h) Pure)

data ReadFile next = ReadFile FilePath (ByteString -> next) deriving (Functor)

type ReadOnlyIO = Free ReadFile

runReadOnlyIO' :: ReadFile next -> IO next
runReadOnlyIO' (ReadFile filePath f) = fmap f (B.readFile filePath)

runReadOnlyIO :: ReadOnlyIO a -> IO a
runReadOnlyIO = foldFree runReadOnlyIO'

-- Named

data Named a b = N String (a -> b)

-- Cached
data Cached m next = C (m ByteString, String) (Named ByteString ByteString) (ByteString -> next) deriving (Functor)
