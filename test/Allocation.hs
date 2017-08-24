{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Lazy hiding (replicate)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Base64 (encode, decode)
import           Data.Stuffed
import Control.DeepSeq (NFData)
import           Prelude              hiding (concat)
import Weigh (mainWith, func)

instance NFData (Stuffed a)

stuffZero :: _ -> Stuffed 0
stuffZero = stuff

main :: IO ()
main = mainWith $ do
    func "stuff/empty" stuffZero ""
    func "stuff/allbytes" stuffZero allBytes
    func "stuff/allzeros" stuffZero zeros
    func "base64encode/empty" encode ""
    func "base64encode/allbytes" encode $ toStrict allBytes
    func "base64encode/allzeros" encode $ toStrict zeros
    func "unstuff/empty" unstuff $ stuffZero ""
    func "unstuff/allbytes" unstuff $ stuffZero allBytes
    func "unstuff/allzeros" unstuff $ stuffZero zeros
    func "base64decode/empty" decode $ encode ""
    func "base64decode/allbytes" decode $ encode $ toStrict allBytes
    func "base64decode/allzeros" decode $ encode $ toStrict zeros
    where
        allBytes = concat $ replicate 40 $ pack [0..255]
        zeros = BSL.replicate 10000 0

