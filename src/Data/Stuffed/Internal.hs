{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Stuffed.Internal
    (IsByte, toLazyByteString)
    where

import Prelude
import Data.ByteString.Builder.Extra (lazyByteStringInsert, toLazyByteStringWith, AllocationStrategy)
import Data.ByteString.Builder.Internal (Builder, newBuffer, customStrategy, smallChunkSize, defaultChunkSize)
import Data.ByteString.Lazy (ByteString)
import GHC.TypeLits (CmpNat, ErrorMessage (..), KnownNat, TypeError)
import GHC.Types (Nat)

type IsByte a = (KnownNat a, IsByteLT a (CmpNat a 256) ~ 'True)

type family IsByteLT (n :: Nat) x where
    IsByteLT _ 'LT = 'True
    IsByteLT n _ = TypeError ('Text "Stuffed can be parametrized only on bytes" ':$$:
                              'Text "(" ':<>: 'ShowType n ':<>: 'Text " is not within range [0, 255]" )

toLazyByteString :: Builder -> ByteString
toLazyByteString = toLazyByteStringWith (myStrategy smallChunkSize defaultChunkSize) ""

{-# INLINE myStrategy #-}
myStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
myStrategy firstSize bufSize =
    customStrategy nextBuffer bufSize (\_ _ -> False)
  where
    {-# INLINE nextBuffer #-}
    nextBuffer Nothing             = newBuffer firstSize
    nextBuffer (Just (old, _)) = return old
