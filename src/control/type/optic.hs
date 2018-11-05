{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Type.Optic where

-- base
import Data.Kind(Type)
import Data.Proxy(Proxy(Proxy))
import Data.Word(Word32)
import GHC.TypeLits( Symbol, KnownSymbol, symbolVal
                   , ErrorMessage(..)
                   )
import GHC.TypeNats(Nat, KnownNat, natVal)

-- fir
import Data.Type.Map(Zip, type (:++:))

----------------------------------------------------------------------

infixr 9 :.:
infixr 9 :%.:
infixr 3 :&:
infixr 3 :%&:

-- optic data (kind)
data Optic where
  -- built-in lenses
  AnIndex :: Optic -- index not known at compile-time
  Index   :: Nat    -> Optic
  Name    :: Symbol -> Optic
  -- lens combinators
  Diag    :: Optic  -> Optic -- equaliser
  (:.:)   :: Optic  -> Optic -> Optic -- composition
  (:&:)   :: Optic  -> Optic -> Optic -- product


-- singletons for optics
data SOptic :: Optic -> Type where
  SAnIndex :: SOptic AnIndex
  SIndex   :: KnownNat n    => Proxy n -> SOptic (Index   n)
  SName    :: KnownSymbol k => Proxy k -> SOptic (Name    k)
  SDiag    :: SOptic l  -> SOptic (Diag l)
  (:%.:)   :: SOptic l1 -> SOptic l2 -> SOptic (l1 :.: l2)
  (:%&:)   :: SOptic l1 -> SOptic l2 -> SOptic (l1 :&: l2)
  

showSOptic :: SOptic optic -> String
showSOptic SAnIndex = "AnIndex"
showSOptic (SIndex n) = "Index " ++ show (natVal    n)
showSOptic (SName  k) = "Name "  ++ show (symbolVal k)
showSOptic (SDiag l) = "Diag " ++ showSOptic l
showSOptic (l1 :%.: l2) = showSOptic l1 ++ " :.: " ++ showSOptic l2
showSOptic (l1 :%&: l2) = showSOptic l1 ++ " :&: " ++ showSOptic l2


class KnownOptic optic where
  opticSing :: SOptic optic
instance KnownOptic AnIndex where
  opticSing = SAnIndex
instance KnownNat n => KnownOptic (Index n) where
  opticSing = SIndex (Proxy @n)
instance KnownSymbol k => KnownOptic (Name k) where
  opticSing = SName (Proxy @k)
instance KnownOptic opt => KnownOptic (Diag opt) where
  opticSing = SDiag (opticSing @opt)  
instance (KnownOptic opt1, KnownOptic opt2)
      => KnownOptic (opt1 :.: opt2) where
  opticSing = (opticSing @opt1) :%.: (opticSing @opt2)
instance (KnownOptic opt1, KnownOptic opt2)
      => KnownOptic (opt1 :&: opt2) where
  opticSing = (opticSing @opt1) :%&: (opticSing @opt2)


----------------------------------------------------------------------
-- dealing with additional information that needs to be passed at runtime
-- this is mostly for passing an array index at runtime,
-- but can be used to pass vector/matrix indices at runtime too

type family RequiredIndices (optic :: Optic) = (r :: [Type]) where
  RequiredIndices (Name  _)   = '[]
  RequiredIndices (Index _)   = '[]
  RequiredIndices AnIndex     = '[Word32]
  RequiredIndices (Diag l)    = RequiredIndices l
  RequiredIndices (l1 :.: l2) = RequiredIndices l1 :++: RequiredIndices l2
  RequiredIndices (l1 :&: l2) -- using (possibly nested) pairs... not a great solution
    = Zip (     Text "Cannot combine optics "
           :<>: ShowType l1 :<>: Text " and " :<>: ShowType l2 :<>: Text "."
           :$$: Text "These optics use different amounts of run-time indices."
          )
        (RequiredIndices l1)
        (RequiredIndices l2) 

----------------------------------------------------------------------
-- type classes

class Gettable (optic :: Optic) obj where
  type Get optic obj :: Type

class Settable (optic :: Optic) obj where
  type Set optic obj :: Type