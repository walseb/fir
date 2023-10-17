{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module FIR.Generics (FromGenericProduct(..)) where

-- base
import Prelude
  hiding ( undefined )
import Data.Typeable
import Data.Kind
  ( Type )
import GHC.TypeLits ( Symbol, KnownSymbol )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( pattern VF )

-- fir
import Data.Type.Map

import FIR.AST.Prim
import FIR.AST.Type

import FIR.Prim.Struct
import FIR.Prim.Types
  ( PrimTyMap )

import FIR.AST

-- generics-sop
import Generics.SOP (NP(..), I(..), K(..))
import qualified Generics.SOP as SOP

------------------------------------------------
-- Generic deriving for Syntactic

-- | Newtype to automatically derive Syntactic for instances generically for product types.
--
-- Example
--
-- @
-- data MinMax = MinMax Float Float
--   deriving (GHC.Generic, SOP.Generic)
--   deriving Syntactic via (FromGenericProduct MinMax ["min", "max"])
-- @
type FromGenericProduct :: Type -> [Symbol] -> Type
newtype FromGenericProduct t xs = FromGenericProduct t

instance forall (t :: Type) (ts :: [Type]) (xs :: [Symbol]) (zgs :: [Symbol :-> Type]) (zgs' :: [Symbol :-> Type]).
         ( SOP.IsProductType t ts

         , zgs  ~ ZipGenericSyntactic  xs ts
         , zgs' ~ ZipGenericSyntactic' xs ts

         , PrimTyMap zgs

         , ASTStructFields AST zgs' zgs
         , StructFResultFromArgs zgs' ~ zgs
         , StructFArgsFromResult zgs AST ~ zgs'

         , SOP.AllZip SyntacticRel xs ts
         , SOP.All Typeable ts
         , SOP.All KnownSymbol xs
         ) => Syntactic (FromGenericProduct t xs) where
  type Internal (FromGenericProduct t xs) = Val (Struct (ZipGenericSyntactic xs (SOP.ProductCode t)))

  toAST :: FromGenericProduct t xs -> AST (Val (Struct (ZipGenericSyntactic xs ts)))
  toAST (FromGenericProduct a)
    = Struct (npToStruct (mkNP @xs) (SOP.productTypeFrom a))
    where
      npToStruct :: forall ns ys
                  . (SOP.AllZip SyntacticRel ns ys)
                 => NP (K ()) ns
                 -> NP I ys
                 -> Struct (ZipGenericSyntactic' ns ys)
      npToStruct Nil   Nil = End
      npToStruct (_ SOP.:* xs) (SOP.I y SOP.:* ys) = toAST y :& npToStruct xs ys

  fromAST :: AST (Val (Struct (ZipGenericSyntactic xs ts))) -> FromGenericProduct t xs
  fromAST (VF (StructF struct)) = FromGenericProduct $
    SOP.productTypeTo @t (structToNp (mkNP @xs) struct)
    where
      structToNp :: forall (ns :: [Symbol]) ys
                  . ( SOP.AllZip SyntacticRel ns ys )
                 => NP (K ()) ns
                 -> Struct (ZipGenericSyntactic' ns ys)
                 -> NP I ys
      structToNp Nil End = Nil
      structToNp ((_ :: (K () (n :: Symbol))) :* xs) (y :& ys)
        = I (fromAST y) :* structToNp xs ys
  fromAST _ = error "impossible"

mkNP :: forall ns. SOP.SListI ns => NP (K ()) ns
mkNP = case SOP.sList @ns of
  SOP.SNil -> Nil
  SOP.SCons -> K () SOP.:* mkNP

type family ZipGenericSyntactic (xs :: [Symbol]) (ys :: [Type]) where
  ZipGenericSyntactic '[] '[] = '[]
  ZipGenericSyntactic (x : xs) (y : ys) = (x ':-> InternalType y) : ZipGenericSyntactic xs ys

type family ZipGenericSyntactic' (xs :: [Symbol]) (ys :: [Type]) where
  ZipGenericSyntactic' '[] '[] = '[]
  ZipGenericSyntactic' (x : xs) (y : ys) = (x ':-> AST (Val y)) : ZipGenericSyntactic' xs ys

class (Val y ~ Internal y, Syntactic y) => SyntacticRel x y
instance (Val y ~ Internal y, Syntactic y) => SyntacticRel x y

