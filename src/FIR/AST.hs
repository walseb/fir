{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.AST

Representation of programs using abstract syntax trees.

The user interface to the AST is through type class overloading,
see "FIR.Syntax.AST" and "FIR.Syntax.Program".

This is an extensible AST represented by an open sum type (variant type),
with a higher-order abstract syntax representation (HOAS),
as seen in the paper /Combining Deep and Shallow Embeddings of Domain-Specific Languages/
by Josef Svenningsson and Emil Axelsson.
-}

module FIR.AST
  ( -- * Main AST data type
    AST, Code
    -- * Syntactic type class
  , Syntactic(Internal, toAST, fromAST)
  , SyntacticVal, InternalType
    -- * Primitive operations
  , primOp
  , HasUndefined(undefined)
    -- * Displaying ASTs graphically
  , toTree

    -- * Re-exports
  , module FIR.AST.ControlFlow
  , module FIR.AST.Effs
  , module FIR.AST.Exts
  , module FIR.AST.Images
  , module FIR.AST.Optics
  , module FIR.AST.Prim

  )
  where

-- base
import Prelude
  hiding ( undefined )
import qualified Prelude
import Data.Kind
  ( Type, Constraint )
import Data.Proxy
  ( Proxy(Proxy) )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( EGADT )

-- tree-view
import Data.Tree.View
  ( showTree )


-- fir
import FIR.AST.ControlFlow
import FIR.AST.Display
  ( toTree )
import FIR.AST.Effs
import FIR.AST.Exts
import FIR.AST.Images
import FIR.AST.Optics
import FIR.AST.Prim
import FIR.AST.Type

import FIR.Prim.Op
  ( PrimOp(PrimOpAugType) )
import {-# SOURCE #-} FIR.Prim.Singletons
  ( PrimTy )

------------------------------------------------------------
-- main AST data type: extensible GADT

type AllOpsF
  = '[ LamF, AppF, LitF, MkIDF, ValueF, UnsafeCoerceF
     , ReturnF, BindF
     , PrimOpF, UndefinedF, GradedMappendF
     , PureF, ApF
     , MkVectorF, MatF, UnMatF, StructF, ArrayF, NilHListF, ConsHListF
     , IfF, IfMF, SwitchF, SwitchMF, WhileF
     , DefF, FunDefF, FunCallF, DefEntryPointF, LocallyF, EmbedF
     , DebugPrintfF
     , ImgOpsF
     , UseF, AssignF, ViewF, SetF
     ]

type AST    = EGADT AllOpsF
type Code a = AST (Val a)

------------------------------------------------

-- | Syntactic type class (Axelsson, Svenningsson).
class Syntactic a where
  type Internal a :: AugType
  toAST :: a -> AST (Internal a)
  fromAST :: AST (Internal a) -> a

type family SyntacticVal a :: Constraint where
  SyntacticVal a = ( Syntactic a, Internal a ~ Val (InternalType a) )
type InternalType a = UnderlyingType (Internal a)

instance Syntactic (AST a) where
  type Internal (AST a) = a
  toAST   = id
  fromAST = id

instance (SyntacticVal a, Syntactic b) => Syntactic (a -> b) where
  type Internal (a -> b) = Internal a :--> Internal b
  toAST        f    = Lam ( toAST . f . fromAST )
  fromAST (Lam f) a = fromAST ( f  $ toAST a )
  fromAST      f  a = fromAST ( f :$ toAST a )

------------------------------------------------

-- | Utility function for defining primops.
primOp :: forall a op r
       .  ( PrimOp op a
          , Syntactic r
          , Internal r ~ PrimOpAugType op a
          )
       => r
primOp = fromAST $ PrimOp ( Proxy @a ) ( Proxy @op )

-- | Types at which we can define "undefined."
class HasUndefined (a :: Type) where
  undefined :: a

instance PrimTy a => HasUndefined (Code a) where
  undefined = Undefined

instance {-# OVERLAPPABLE #-} HasUndefined a where
  undefined = Prelude.undefined

------------------------------------------------
-- display AST for viewing

instance Show (AST ast) where
  show = showTree . toTree
