{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module: Data.Finite.With

Allows a function expecting a type-level natural to be passed a 'Data.Finite.Finite'.
-}


module Data.Finite.With
  ( withFinite )
  where

-- base
import Data.Proxy
  ( Proxy )
import Data.Type.Equality
  ( (:~:)(Refl) )
import GHC.TypeNats
  ( Nat, KnownNat
  , SomeNat(..), someNatVal
  , CmpNat
  )
import Unsafe.Coerce
  ( unsafeCoerce )

-- finite-typelits
import Data.Finite
  ( Finite, getFinite )

----------------------------------------------------------------------

-- | Pass a 'Finite' to a function expecting a type-level natural (passed via a 'Proxy').
withFinite
  :: forall (n :: Nat) r
  .  ( forall i. ( KnownNat i, CmpNat i n ~ 'LT ) => Proxy i -> r )
  -> Finite n
  -> r
withFinite f j = case someNatVal ( fromIntegral $ getFinite j ) of
  ( SomeNat ( px :: Proxy j ) ) ->
    case unsafeCoerce Refl :: CmpNat j n :~: 'LT of
      Refl -> f px
