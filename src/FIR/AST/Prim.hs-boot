{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RoleAnnotations #-}

module FIR.AST.Prim where

-- base
import Data.Kind
  ( Type )

-- fir
import FIR.AST.Type
  ( AugType )

------------------------------------------------------------

data LamF           ( ast :: AugType -> Type ) ( t :: AugType )
data AppF           ( ast :: AugType -> Type ) ( t :: AugType )
data LitF           ( ast :: AugType -> Type ) ( t :: AugType )
data MkIDF          ( ast :: AugType -> Type ) ( t :: AugType )
data ValueF         ( ast :: AugType -> Type ) ( t :: AugType )
data UnsafeCoerceF  ( ast :: AugType -> Type ) ( t :: AugType )
data ReturnF        ( ast :: AugType -> Type ) ( t :: AugType )
data BindF          ( ast :: AugType -> Type ) ( t :: AugType )
data PrimOpF        ( ast :: AugType -> Type ) ( t :: AugType )
data UndefinedF     ( ast :: AugType -> Type ) ( t :: AugType )
data GradedMappendF ( ast :: AugType -> Type ) ( t :: AugType )
data PureF          ( ast :: AugType -> Type ) ( t :: AugType )
data ApF            ( ast :: AugType -> Type ) ( t :: AugType )
data MkVectorF      ( ast :: AugType -> Type ) ( t :: AugType )
data MatF           ( ast :: AugType -> Type ) ( t :: AugType )
data UnMatF         ( ast :: AugType -> Type ) ( t :: AugType )
data StructF        ( ast :: AugType -> Type ) ( t :: AugType )
data ArrayF         ( ast :: AugType -> Type ) ( t :: AugType )
data ConsHListF     ( ast :: AugType -> Type ) ( t :: AugType )
data NilHListF      ( ast :: AugType -> Type ) ( t :: AugType )

type role LamF           representational nominal
type role AppF           representational nominal
type role LitF           phantom          nominal
type role MkIDF          phantom          nominal
type role ValueF         phantom          nominal
type role UnsafeCoerceF  phantom          nominal
type role ReturnF        phantom          nominal
type role BindF          phantom          nominal
type role PrimOpF        phantom          nominal
type role UndefinedF     phantom          nominal
type role GradedMappendF phantom          nominal
type role PureF          representational nominal
type role ApF            representational nominal
type role MkVectorF      representational nominal
type role MatF           phantom          nominal
type role UnMatF         phantom          nominal
type role StructF        nominal          nominal
type role ArrayF         nominal          nominal
type role ConsHListF     phantom          nominal
type role NilHListF      phantom          nominal
