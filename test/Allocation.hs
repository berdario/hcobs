{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Lazy hiding (replicate)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Base64 (encode, decode)
import           Data.Stuffed
import Control.DeepSeq --(NFData)
import Control.Exception
import           Prelude              hiding (concat)
import Weigh (mainWith, func)

instance NFData (Stuffed a)

stuffZero :: _ -> Stuffed 0
stuffZero = stuff

main :: IO ()
main = do
    let stuffedAll = stuffZero allBytes
    let stuffedZeros = stuffZero zeros
    let base64All = encode $ toStrict allBytes
    let base64Zeros = encode $ toStrict zeros
    evaluate $!! stuffZero allBytes
    evaluate $!! stuffZero zeros
    evaluate $!! encode $ toStrict allBytes
    evaluate $!! encode $ toStrict zeros
    mainWith $ do
        func "stuff/empty" stuffZero ""
        func "stuff/allbytes" stuffZero allBytes
        func "stuff/allzeros" stuffZero zeros
        func "base64encode/empty" encode ""
        func "base64encode/allbytes" encode $ toStrict allBytes
        func "base64encode/allzeros" encode $ toStrict zeros
        func "unstuff/empty" unstuff $ stuffZero ""
        func "unstuff/allbytes" unstuff stuffedAll
        func "unstuff/allzeros" unstuff stuffedZeros
        func "base64decode/empty" decode $ encode ""
        func "base64decode/allbytes" decode base64All
        func "base64decode/allzeros" decode base64Zeros
        where
            allBytes = concat $ replicate 40 $ pack [0..255]
            zeros = BSL.replicate 10000 0

