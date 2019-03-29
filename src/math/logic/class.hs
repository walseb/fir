{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Math.Logic.Class where

-- base
import Prelude
  ( Bool(..)
  , Int, Word, Float, Double
  , Semigroup, Monoid
  , Foldable(..)
  , flip
  )
import qualified Prelude
import Data.Coerce
  ( Coercible, coerce )
import Data.Int
  ( Int8,Int16,Int32,Int64 )
import Data.Kind
  ( Type )
import Data.Word
  ( Word8,Word16,Word32,Word64 )
import Foreign.C.Types
import qualified GHC.Stack

-- half
import Numeric.Half
  ( Half )

-- fir
import Deriving.Base
  ( Base(..) ) -- newtype for deriving via base instances

----------------------------------------------------------------------

infixr 3 &&
infixr 2 ||

class Boolean b where
  true  :: b
  false :: b
  (&&)  :: b -> b -> b
  (||)  :: b -> b -> b
  not   :: b -> b

instance Boolean Bool where
  true  = True
  false = False
  (&&)  = (Prelude.&&)
  (||)  = (Prelude.||)
  not   = Prelude.not

infix 4 ==
infix 4 /=

type family Choosing b (t :: (Type,Type,Type)) = r | r -> b t where
  Choosing b '(x,y,z) = b -> x -> y -> z

class Boolean b => Choose b (t :: (Type,Type,Type)) where
  choose :: Choosing b t

ifThenElse :: (GHC.Stack.HasCallStack, Choose b (t :: (Type,Type,Type)))
           => Choosing b t
ifThenElse = choose

type Triple a = '(a,a,a)

instance Choose Bool (Triple a) where
  choose True  x _ = x
  choose False _ y = y

class Boolean (Logic a) => Eq a where
  type Logic a
  (==) :: a -> a -> Logic a
  (/=) :: a -> a -> Logic a
  a /= b = not (a == b)

instance Prelude.Eq a => Eq (Base a) where
  type Logic (Base a) = Bool
  (==) = coerce ( (Prelude.==) :: a -> a -> Bool )
  (/=) = coerce ( (Prelude./=) :: a -> a -> Bool )

deriving via Base ()     instance Eq ()
deriving via Base Bool   instance Eq Bool
deriving via Base Word8  instance Eq Word8
deriving via Base Word16 instance Eq Word16
deriving via Base Word32 instance Eq Word32
deriving via Base Word64 instance Eq Word64
deriving via Base Word   instance Eq Word
deriving via Base Int8   instance Eq Int8
deriving via Base Int16  instance Eq Int16
deriving via Base Int32  instance Eq Int32
deriving via Base Int64  instance Eq Int64
deriving via Base Int    instance Eq Int
deriving via Base Half   instance Eq Half
deriving via Base Float  instance Eq Float
deriving via Base Double instance Eq Double

deriving via Base CChar   instance Eq CChar
deriving via Base CSChar  instance Eq CSChar
deriving via Base CUChar  instance Eq CUChar
deriving via Base CShort  instance Eq CShort
deriving via Base CUShort instance Eq CUShort
deriving via Base CInt    instance Eq CInt
deriving via Base CUInt   instance Eq CUInt
deriving via Base CLong   instance Eq CLong
deriving via Base CULong  instance Eq CULong
deriving via Base CLLong  instance Eq CLLong
deriving via Base CULLong instance Eq CULLong
deriving via Base CFloat  instance Eq CFloat
deriving via Base CDouble instance Eq CDouble


newtype All b = All { getAll :: b }
newtype Any b = Any { getAny :: b }

instance Boolean b => Semigroup (All b) where
  (<>) = coerce ( (&&) @b )
instance Boolean b => Monoid (All b) where
  mempty = coerce ( true @b )

instance Boolean b => Semigroup (Any b) where
  (<>) = coerce ( (||) @b )
instance Boolean b => Monoid (Any b) where
  mempty = coerce (false @b)

-- from Data.Functor.Utils
{-# INLINE (#.) #-}
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce

and :: (Foldable t, Boolean b) => t b -> b
and = getAll #. foldMap All

or :: (Foldable t, Boolean b) => t b -> b
or = getAny #. foldMap Any

any :: (Foldable t, Boolean b) => (a -> b) -> t a -> b
any p = getAny #. foldMap (Any #. p)

all :: (Foldable t, Boolean b) => (a -> b) -> t a -> b
all p = getAll #. foldMap (All #. p)

infix 4 <=
infix 4 <
infix 4 >=
infix 4 >

class Eq a => Ord a where
  type Ordering a
  compare :: a -> a -> Ordering a
  (<=)    :: a -> a -> Logic a
  (>=)    :: a -> a -> Logic a
  (<)     :: a -> a -> Logic a
  (>)     :: a -> a -> Logic a
  max     :: a -> a -> a
  min     :: a -> a -> a

  (>=) = flip (>=)
  (>)  = flip (<)


instance Prelude.Ord a => Ord (Base a) where
  type Ordering (Base a) = Prelude.Ordering
  compare = coerce ( Prelude.compare :: a -> a -> Prelude.Ordering )
  (<=)    = coerce ( (Prelude.<=)    :: a -> a -> Bool )
  (>=)    = coerce ( (Prelude.>=)    :: a -> a -> Bool )
  (<)     = coerce ( (Prelude.<)     :: a -> a -> Bool )
  (>)     = coerce ( (Prelude.>)     :: a -> a -> Bool )
  max     = coerce ( Prelude.max     :: a -> a -> a )
  min     = coerce ( Prelude.min     :: a -> a -> a )

deriving via Base ()     instance Ord ()
deriving via Base Bool   instance Ord Bool
deriving via Base Word8  instance Ord Word8
deriving via Base Word16 instance Ord Word16
deriving via Base Word32 instance Ord Word32
deriving via Base Word64 instance Ord Word64
deriving via Base Word   instance Ord Word
deriving via Base Int8   instance Ord Int8
deriving via Base Int16  instance Ord Int16
deriving via Base Int32  instance Ord Int32
deriving via Base Int64  instance Ord Int64
deriving via Base Int    instance Ord Int
deriving via Base Half   instance Ord Half
deriving via Base Float  instance Ord Float
deriving via Base Double instance Ord Double

deriving via Base CChar   instance Ord CChar
deriving via Base CSChar  instance Ord CSChar
deriving via Base CUChar  instance Ord CUChar
deriving via Base CShort  instance Ord CShort
deriving via Base CUShort instance Ord CUShort
deriving via Base CInt    instance Ord CInt
deriving via Base CUInt   instance Ord CUInt
deriving via Base CLong   instance Ord CLong
deriving via Base CULong  instance Ord CULong
deriving via Base CLLong  instance Ord CLLong
deriving via Base CULLong instance Ord CULLong
deriving via Base CFloat  instance Ord CFloat
deriving via Base CDouble instance Ord CDouble
