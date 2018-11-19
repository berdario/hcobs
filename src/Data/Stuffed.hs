{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}

-- | This module tries to be as efficient as possible, type safe and easy to use. If you have a "sink" like
--
-- > sink :: Stuffed 0 -> IO ()
-- > sink = undefined
--
-- You'd then simply be able to encode a Bytestring with
--
-- > sink $ stuff bytes
--
-- You can try this out in ghci with:
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> import Data.Stuffed
-- >>> let stuffedBytes = stuff "a\0b\0c" :: Stuffed 0
-- >>> unpack $ unwrap stuffedBytes -- directly access the underlying bytestring
-- [2,97,2,98,2,99]
-- >>> unpack $ unstuff stuffedBytes
-- [97,0,98,0,99]

module Data.Stuffed
    ( Stuffed
    , stuff
    , unstuff
    , unwrap
    )
    where

import qualified Data.ByteString          as B
import           Data.ByteString.Builder  (Builder, byteString, toLazyByteString)
import           Data.ByteString.Lazy     (ByteString, fromChunks, splitAt,
                                           toStrict)
import           Data.Int                 (Int64)
import           Data.Proxy               (Proxy (..))
import           Data.Reflection          (reflect)
import           Data.Semigroup           (Semigroup)
import           Data.Word                (Word8)
import           GHC.Generics             (Generic)
import           GHC.Types                (Nat)
import           Prelude                  hiding (concat, length, null, splitAt, last)

import           Data.Stuffed.Internal    (IsByte)

-- | Wrapper for Lazy Bytestrings, parametrized on the Byte (Word8, represented as a type-level 'Nat') to be encoded away.
newtype Stuffed (a :: Nat) = Stuffed ByteString
    deriving (Eq, Ord, Show, Semigroup, Monoid, Generic)

splitEvery :: Int64 -> ByteString -> [B.ByteString]
splitEvery _ "" = []
splitEvery n bs = toStrict start : splitEvery n rest
    where
        (start, rest) = splitAt n bs

stuff :: forall a. IsByte a => ByteString -> Stuffed a
stuff bs = Stuffed $ fromChunks chunks
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        chunks = map (buildStuffed excluded) $ splitEvery 254 bs

buildStuffed :: Word8 -> B.ByteString -> B.ByteString
buildStuffed excluded bs = B.cons last stuffed
    where
        excludedOffset = excluded + 1
        swapExcluded n current | current == excluded = (excludedOffset, n)
                               | otherwise = (n + 1, current)
        (last, stuffed) = B.mapAccumR swapExcluded excludedOffset bs

unstuff :: forall a. IsByte a => Stuffed a -> ByteString
unstuff (Stuffed bs) = toLazyByteString $ mconcat chunks
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        rebuild bytes = rebuildChunk excluded (B.uncons bytes) mempty
        chunks = map rebuild $ splitEvery 255 bs

rebuildChunk :: Word8 -> Maybe (Word8, B.ByteString) -> Builder -> Builder
rebuildChunk _ Nothing b = b
rebuildChunk excluded (Just (starting_offset, rest)) _ = byteString $ snd $ B.mapAccumL swapExcluded (starting_offset - 1) rest
    where
        swapExcluded n current | n == excluded = (current - 1, excluded)
                               | otherwise = (n - 1, current)

-- | Extract the encoded bytestring from a Stuffed
unwrap :: Stuffed a -> ByteString
unwrap (Stuffed bs) = bs
