{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Monad (unless)
import Control.Monad.Identity (Identity)
import Control.Monad.Morph    (generalize, hoist)
import Data.ByteString.Lazy (ByteString, length, notElem, fromStrict)
import Data.Proxy (Proxy(..))
import Data.Reflection (Reifies, reflect, reifyNat)
import Data.Word (Word8)
import GHC.Exts (Constraint)
import GHC.TypeLits (KnownNat, CmpNat)
import Hedgehog
import Hedgehog.Gen (bytes, word8)
import Hedgehog.Range (constant, constantBounded)
import Prelude hiding (length, notElem)
import System.Exit (exitFailure)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe

import Data.Stuffed (Stuffed, stuff, unstuff, unwrap)

data Dict (c :: Constraint) where
    Dict :: (c) => Dict c

main :: IO ()
main = do
    result <- checkParallel $ Group "Stuffed properties"
                    [ ("roundtrips",
                        stuffedProperty (bytesAndExclusion 0) (const roundTrips))
                    , ("1 byte bigger every 254",
                        withTests 1000 $
                        stuffedProperty (bytesAndExclusion 1) (const biggerBy1Every254))
                    , ("Doesn't contain the excluded byte",
                        stuffedProperty (bytesAndExclusion 0) notContains) ]
    unless result exitFailure

type Generator = Gen (ByteString, Word8)

bytesAndExclusion :: Int -> Generator
bytesAndExclusion minSize = (,) <$> (fromStrict <$> bytes (constant minSize 1100)) <*> word8 constantBounded

smallerThan256 :: forall a. (KnownNat a) => Maybe (Dict (KnownNat a, CmpNat a 256 ~ 'LT))
smallerThan256 = if n < 256
        then Just $ unsafeCoerce (Dict :: Dict (KnownNat a, 'LT ~ 'LT))
        else Nothing
    where
        n = reflect (Proxy :: Proxy a)

stuffedProperty :: Generator -> (forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => Word8 -> ByteString -> Proxy a -> PropertyT Identity ()) -> Property
stuffedProperty gen prop = property $ hoist generalize $ do
    (x, w) <- forAll gen
    reifyNat (fromIntegral w) $ \(p :: Proxy n) ->
        case smallerThan256 of
            Nothing -> failure
            (Just (Dict :: Dict (KnownNat n, CmpNat n 256 ~ 'LT))) -> prop w x p



roundTrips :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => ByteString -> Proxy a -> PropertyT Identity ()
roundTrips bs _ = bs === unstuff (stuff bs :: Stuffed a)

biggerBy1Every254 :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => ByteString -> Proxy a -> PropertyT Identity ()
biggerBy1Every254 bs _ = length stf === inputL + ((inputL `div` 255) + inputL) `div` 255 + 1
    where
        stf = unwrap (stuff bs :: Stuffed a)
        inputL = length bs


notContains :: forall a. (KnownNat a, CmpNat a 256 ~ 'LT) => Word8 -> ByteString -> Proxy a -> PropertyT Identity ()
notContains i bs _ = assert $ i `notElem` stf
    where
        stf = unwrap (stuff bs :: Stuffed a)

