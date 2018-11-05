{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Function.Variadic where

-- base
import Data.Kind(Type)
import GHC.TypeNats(Nat, type (-), type (<=?))      

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