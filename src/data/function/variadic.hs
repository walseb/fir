{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Function.Variadic where

-- base
import Data.Int(Int8,Int16,Int32,Int64)
import Data.Kind(Type)
import Data.Word(Word8,Word16,Word32,Word64)
import GHC.TypeNats(Nat, type (-), type (<=?))

-- half
import Numeric.Half(Half)

-- fir
import {-# SOURCE #-} FIR.PrimTy(Array,RuntimeArray,Struct)
import Math.Linear(V,M)

------------------------------------------------------------

type family NatVariadic
              ( n :: Nat  )
              ( a :: Type )
              ( b :: Type )
            = ( r :: Type )
            where
  NatVariadic n a b = NatVariadic' n a b (1 <=? n)

type family NatVariadic'
              ( n    :: Nat  )
              ( a    :: Type )
              ( b    :: Type )
              ( geq1 :: Bool )
            = ( r    :: Type )
            where
  NatVariadic' _ _ b 'False = b
  NatVariadic' n a b 'True  = a -> NatVariadic (n-1) a b

------------------------------------------------------------

-- explicitly list all primitive types, to allow injectivity annotation

type family ListVariadic (as :: [Type]) (b :: Type) = (r :: Type) | r -> as b where  
  ListVariadic (a ': as) b = a -> ListVariadic as b
  ListVariadic '[] ()     = ()
  ListVariadic '[] Bool   = Bool
  ListVariadic '[] Word   = Word 
  ListVariadic '[] Word8  = Word8
  ListVariadic '[] Word16 = Word16
  ListVariadic '[] Word32 = Word32
  ListVariadic '[] Word64 = Word64
  ListVariadic '[] Int    = Int 
  ListVariadic '[] Int8   = Int8
  ListVariadic '[] Int16  = Int16
  ListVariadic '[] Int32  = Int32
  ListVariadic '[] Int64  = Int64
  ListVariadic '[] Half   = Half
  ListVariadic '[] Float  = Float
  ListVariadic '[] Double = Double
  ListVariadic '[] (V n a) = V n a
  ListVariadic '[] (M m n a) = M m n a
  ListVariadic '[] (Struct as) = Struct as
  ListVariadic '[] (Array n a) = Array n a
  ListVariadic '[] (RuntimeArray a) = RuntimeArray a