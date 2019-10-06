{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module FIR.Layout
  ( Layout(..)
  , Poke(..)
  )
  where

-- base 
import Data.Word
  ( Word32 )
import Foreign.Ptr
  ( Ptr )
import GHC.TypeNats
  ( Nat, KnownNat )

-- fir
import Data.Type.Known
  ( knownValue )

--------------------------------------------------------------------------------------------

data Layout
  = Base
  | Extended
  | Locations

class ( KnownNat (SizeOf    lay a)
      , KnownNat (Alignment lay a)
      ) => Poke a (lay :: Layout) where

  type SizeOf lay a :: Nat
  sizeOf :: Word32
  sizeOf = knownValue @(SizeOf lay a)

  type Alignment lay a :: Nat
  alignment :: Word32
  alignment = knownValue @(Alignment lay a)

  poke :: Ptr a -> a -> IO ()
