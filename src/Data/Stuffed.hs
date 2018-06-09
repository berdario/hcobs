{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}

module Data.Stuffed
    ( Stuffed
    , stuff
    , unstuff
    , unwrap
    )
    where

import qualified Data.ByteString          as B
import           Data.ByteString.Builder  (Builder, byteString, lazyByteString,
                                           shortByteString, toLazyByteString,
                                           word8)
import           Data.ByteString.Internal (memcpy, unsafeCreate)
import qualified Data.ByteString.Internal as BI
import           Data.ByteString.Lazy     (ByteString, fromChunks, fromStrict,
                                           length, null, split, splitAt,
                                           toStrict, uncons)
import           Data.Int                 (Int64)
import           Data.Monoid              ((<>))
import           Data.Proxy               (Proxy (..))
import           Data.Reflection          (reflect)
import           Data.Word                (Word8)
import           GHC.Generics             (Generic)
import           GHC.TypeLits             (CmpNat, KnownNat)
import           GHC.Types                (Nat)
import           Prelude                  hiding (concat, length, null, splitAt)


newtype Stuffed (a :: Nat) = Stuffed ByteString
    deriving (Eq, Ord, Show, Monoid, Generic)

splitEvery :: Int64 -> ByteString -> [B.ByteString]
splitEvery _ "" = []
splitEvery n bs = toStrict start : splitEvery n rest
    where
        (start, rest) = splitAt n bs

stuff :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => ByteString -> Stuffed a
stuff bs = Stuffed $ fromChunks chunks
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        chunks = map (buildStuffed excluded) $ splitEvery 254 bs

buildStuffed :: Word8 -> B.ByteString -> B.ByteString
buildStuffed excluded bs = B.cons last stuffed
    where
        excludedOffset = fromIntegral excluded + 1
        swapExcluded n current | current == excluded = (excludedOffset, n)
                               | otherwise = (n + 1, current)
        (last, stuffed) = B.mapAccumR swapExcluded excludedOffset bs

toOffset :: Word8 -> Word8 -> Int64
toOffset excluded b = fromIntegral $ b - 1 - excluded

unstuff :: forall a. (KnownNat a) => Stuffed a -> ByteString
unstuff (Stuffed bs) = toLazyByteString $ mconcat chunks
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        rebuild bytes = rebuildChunk excluded (B.uncons bytes) mempty
        chunks = map rebuild $ splitEvery 255 bs

rebuildChunk :: Word8 -> Maybe (Word8, B.ByteString) -> Builder -> Builder
rebuildChunk _ Nothing b = b
rebuildChunk excluded (Just (starting_offset, rest)) b = byteString $ snd $ B.mapAccumL swapExcluded (starting_offset - 1) rest
    where
        swapExcluded n current | n == excluded = (current - 1, excluded)
                               | otherwise = (n - 1, current)

unwrap :: Stuffed a -> ByteString
unwrap (Stuffed bs) = bs
