# hcobs

[![Build Status](https://travis-ci.org/berdario/hcobs.svg?branch=master)](https://travis-ci.org/berdario/hcobs)

Haskell implementation of the [Consistent Overhead Byte Stuffing](https://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing) algorithm.

It provides a `Stuffed` newtype wrapper for Lazy Bytestrings, which is parametrized on the Byte (Word8, represented as a type-level Nat) to be encoded away.

The implementation tries to be as efficient as possible, type safe and easy to use. If you have a "sink" like


    sink :: Stuffed 0 -> IO ()
    sink = undefined

You'd then simply be able to encode a Bytestring with `sink $ stuff bytes`.

You can try this out in ghci with:

    > :set -XOverloadedStrings
    > :set -XDataKinds
    > import Data.Stuffed
    > let stuffedBytes = stuff "a\0b\0c" :: Stuffed 0
    > unpack $ unwrap stuffedBytes -- directly access the underlying bytestring
    [2,97,2,98,2,99]
    > unpack $ unstuff stuffedBytes
    [97,0,98,0,99]
