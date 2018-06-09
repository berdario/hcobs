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
    , stuff'
    , stuff''
    , stuff'''
    , unstuff
    , unwrap
    )
    where

-- import           Control.Monad.ST.Trans   (newSTRef, runSTT)
import           Control.Monad            (foldM_)
import qualified Data.ByteString          as B
import           Data.ByteString.Builder  (Builder, byteString, lazyByteString,
                                           shortByteString, toLazyByteString,
                                           word8)
import           Data.ByteString.Internal (memcpy, unsafeCreate)
import qualified Data.ByteString.Internal as BI
import           Data.ByteString.Lazy     (ByteString, fromChunks, fromStrict,
                                           length, null, split, splitAt,
                                           toStrict, uncons)
import           Data.ByteString.Short    (toShort)
import           Data.Foldable            (forM_)
import           Data.Int                 (Int64)
import           Data.List                (scanl')
import           Data.Monoid              ((<>))
import           Data.Proxy               (Proxy (..))
import           Data.Reflection          (reflect)
import           Data.Word                (Word8)
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (plusPtr)
import           Foreign.Storable         (poke)
import           GHC.Generics             (Generic)
import           GHC.Ptr                  (Ptr)
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

splitEvery' :: Int64 -> ByteString -> [ByteString]
splitEvery' n' bs' = reverse $ go n' bs' []
    where
        go _ "" xs = xs
        go n bs xs = go n rest (start : xs)
            where
                (start, rest) = splitAt n bs

stuff :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => ByteString -> Stuffed a
stuff bs = Stuffed $ toLazyByteString $ mconcat chunks
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        chunks = map (buildStuffed excluded) $ splitEvery 254 bs

buildStuffed :: Word8 -> B.ByteString -> Builder
buildStuffed w bs = foldl (\b a -> b <> word8 (offset a) <> byteString (a :: B.ByteString)) mempty chunks
    where
        chunks = B.split w bs :: [B.ByteString]
        offset = fromOffset w . B.length

stuff' :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => ByteString -> Stuffed a
stuff' bs = Stuffed $ fromChunks chunks'
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        chunks = splitEvery 254 bs
        chunks' = map (buildStuffed' excluded) chunks

buildStuffed' :: Word8 -> B.ByteString -> B.ByteString
buildStuffed' w bs = unsafeCreate (l + 1) $ \ptr ->
            forM_ (zip offsets chunks) $ \(prevLen, BI.PS fp off len) -> do
                let p = plusPtr ptr $ fromIntegral prevLen
                poke p $ offset len
                withForeignPtr fp $ \bytesp -> memcpy (p `plusPtr` 1) (bytesp `plusPtr` off) len
        where
            chunks = B.split w bs
            offsets = scanl' (\a b -> a + 1 + B.length b) 0 chunks
            offset = fromOffset w
            l = B.length bs

stuff'' :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => ByteString -> Stuffed a
stuff'' bs = Stuffed $ fromStrict $ unsafeCreate finalLength $ \ptr ->
    foldM_ (buildStuffed'' excluded) ptr chunks
    where
        inputL = fromIntegral $ length bs
        finalLength = inputL + ((inputL `div` 255) + inputL) `div` 255 + 1
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        chunks = splitEvery 254 bs

buildStuffed'' :: Word8 -> Ptr Word8 -> B.ByteString -> IO (Ptr Word8)
buildStuffed'' w ptr bs = do
            forM_ (zip offsets chunks) $ \(prevLen, BI.PS fp off len) -> do
                let p = plusPtr ptr $ fromIntegral prevLen
                poke p $ offset len
                withForeignPtr fp $ \bytesp -> memcpy (p `plusPtr` 1) (bytesp `plusPtr` off) len
            return (ptr `plusPtr` fromIntegral (last offsets))
        where
            chunks = B.split w bs
            offsets = scanl (\a b -> a + 1 + B.length b) 0 chunks
            offset = fromOffset w
            l = B.length bs

stuff''' :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => ByteString -> Stuffed a
stuff''' bs = Stuffed $ fromChunks chunks'
    where
        excluded = fromIntegral (reflect (Proxy :: Proxy a) :: Integer)
        chunks = splitEvery 254 bs
        chunks' = map (buildStuffed''' excluded) chunks

buildStuffed''' :: Word8 -> B.ByteString -> B.ByteString
buildStuffed''' excluded bs = B.cons last stuffed
    where
        excludedOffset = fromIntegral excluded + 1
        swapExcluded n current | current == excluded = (excludedOffset, n)
                               | otherwise = (n + 1, current)
        (last, stuffed) = B.mapAccumR swapExcluded excludedOffset bs


fromOffset :: (Integral a) => Word8 -> a -> Word8
fromOffset excluded n = fromIntegral n + 1 + excluded

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
rebuildChunk excluded (Just (ovhd, rest)) b = rebuildChunk excluded (B.uncons after) $ b <> byteString before <> nextExcluded
    where
        (before, after) = B.splitAt (fromIntegral $ toOffset excluded ovhd) rest
        nextExcluded = if B.null after then mempty else word8 excluded


unwrap :: Stuffed a -> ByteString
unwrap (Stuffed bs) = bs
