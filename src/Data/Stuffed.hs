{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Data.Stuffed
    ( Stuffed
    , stuff
    , unstuff
    , unwrap
    )
    where

import Data.ByteString.Lazy (ByteString, split, length, uncons, splitAt, null)
import Data.ByteString.Builder (Builder, word8, shortByteString, lazyByteString, toLazyByteString)
import Data.ByteString.Short (toShort)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Monoid ((<>))
import Data.Word (Word8)
import Data.Reflection (reflect)
import GHC.TypeLits (KnownNat, CmpNat)
import GHC.Types (Nat)
import Prelude hiding (null, length, splitAt)


newtype Stuffed (a :: Nat) = Stuffed ByteString
    deriving (Eq, Ord, Show, Monoid)

stuff :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => ByteString -> Stuffed a
stuff bs = Stuffed $ toLazyByteString $ mconcat chunks
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        chunks = map (buildStuffed excluded) $ splitEvery 254 bs

splitEvery :: Int64 -> ByteString -> [ByteString]
splitEvery _ "" = []
splitEvery n bs = start : splitEvery n rest
    where
        (start, rest) = splitAt n bs

buildStuffed :: Word8 -> ByteString -> Builder
buildStuffed w bs = foldl (\b a -> b <> word8 (offset a) <> lazyByteString a) mempty chunks
    where
        chunks = split w bs
        offset = fromOffset w . length

fromOffset :: Word8 -> Int64 -> Word8
fromOffset excluded n = fromIntegral n + 1 + excluded

toOffset :: Word8 -> Word8 -> Int64
toOffset excluded b = fromIntegral $ b - 1 - excluded

unstuff :: forall a. (KnownNat a) => Stuffed a -> ByteString
unstuff (Stuffed bs) = toLazyByteString $ mconcat chunks
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        rebuild bytes = rebuildChunk excluded (uncons bytes) mempty
        chunks = map rebuild $ splitEvery 255 bs

rebuildChunk :: Word8 -> Maybe (Word8, ByteString) -> Builder -> Builder
rebuildChunk _ Nothing b = b
rebuildChunk excluded (Just (ovhd, rest)) b = rebuildChunk excluded (uncons after) $ b <> lazyByteString before <> nextExcluded
    where
        (before, after) = splitAt (toOffset excluded ovhd) rest
        nextExcluded = if null after then mempty else word8 excluded


unwrap :: Stuffed a -> ByteString
unwrap (Stuffed bs) = bs

