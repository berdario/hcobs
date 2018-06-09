{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Criterion.Main
import Data.ByteString (useAsCStringLen, packCStringLen)
import           Data.ByteString.Lazy hiding (replicate)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Base64 (encode, decode)
import           Data.Stuffed
import           Prelude              hiding (concat)
import           System.IO.Unsafe     (unsafePerformIO)

stuffedRt = unstuff . (stuff :: _ -> Stuffed 0)
cStringRt = fromStrict . unsafePerformIO . flip useAsCStringLen packCStringLen . toStrict
base64Rt = fromStrict . (\(Right x) -> x) . decode . encode . toStrict

benchStuffing :: (ByteString -> ByteString) -> [Benchmark]
benchStuffing roundTrip =
    [ bench "empty" $ nf roundTrip ""
    , bench "allbytes" $ nf roundTrip $ concat $ replicate 40 $ pack [0..255]
    , bench "allzeros" $ nf roundTrip $ BSL.replicate 10000 0
    ]

main = defaultMain [
      bgroup "stuff/unstuff" $ benchStuffing stuffedRt
    , bgroup "base64" $ benchStuffing base64Rt
    , bgroup "asCString/packCString" $ benchStuffing cStringRt
    ]
