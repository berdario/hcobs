{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

module Data.Stuffed.Internal
    (IsByte)
    where

import Prelude
import GHC.TypeLits (CmpNat, ErrorMessage (..), KnownNat, TypeError)
import GHC.Types (Nat)

type IsByte a = (KnownNat a, IsByteLT a (CmpNat a 256) ~ 'True)

type family IsByteLT (n :: Nat) x where
    IsByteLT _ 'LT = 'True
    IsByteLT n _ = TypeError ('Text "Stuffed can be parametrized only on bytes" ':$$:
                              'Text "(" ':<>: 'ShowType n ':<>: 'Text " is not within range [0, 255]" )

