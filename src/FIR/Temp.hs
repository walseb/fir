{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Temp

Functionality for creating new (temporary) variable names at the type level. Currently unused.
-}

module FIR.Temp
  ( -- * Creating fresh temporary variable names
    NewTemp
  ) where

-- base
import Data.Type.Bool
  ( If )
import GHC.TypeLits
  ( Symbol, CmpSymbol, AppendSymbol )
import GHC.TypeNats
  ( type (+) )

-- fir
import Data.Type.Map
  ( (:->)((:->)) )
import Data.Type.String
  ( ShowNat, ReadPrefixedNat )

-------------------------------------------------------------------------------------------
-- Creating fresh temporary variable names

type TempPrefix = ( "__tmp" :: Symbol )

-- | Create a new temporary variable name.
type family NewTemp (i :: [Symbol :-> t]) :: Symbol where
  NewTemp i = NextTemp (LastTemp i)

type family NextTemp ( mbk :: Maybe Symbol ) :: Symbol where
  NextTemp Nothing = TempPrefix `AppendSymbol` "0001"
  NextTemp (Just k)
    = TempPrefix `AppendSymbol` ( ShowNat (ReadPrefixedNat TempPrefix k + 1) )

--type family ReadTempNat ( k :: Symbol ) :: Nat where
--  ReadTempNat k = ReadPrefixedNat TempPrefix k

type family LastTemp (i :: [Symbol :-> t]) :: Maybe Symbol where
  LastTemp '[] = Nothing
  LastTemp ( (k ':-> _) ': ks )
    = If ( InTempRange k )
        ( Just (LastInRange k ks) )
        ( LastTemp ks )

type family LastInRange (k :: Symbol) (i :: [Symbol :-> t]) :: Symbol where
  LastInRange k '[] = k
  LastInRange k ( (l ':-> _) ': ls)
    = If ( InTempRange l )
        ( LastInRange l ls )
        k

type family InTempRange (k :: Symbol) :: Bool where
  InTempRange k
    = InBetween
        ( CmpSymbol k (TempPrefix `AppendSymbol` "0") )
        ( CmpSymbol k (TempPrefix `AppendSymbol` ":") )

type family InBetween (min :: Ordering) (max :: Ordering) :: Bool where
  InBetween GT LT = True
  InBetween _  _  = False
