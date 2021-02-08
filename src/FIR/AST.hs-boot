{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module FIR.AST
  ( AST, Code
  , Syntactic(..)
  , InternalType
  )
  where

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( EGADT )

-- fir
import FIR.AST.Type
  ( AugType(Val), UnderlyingType )
import {-# SOURCE #-} FIR.AST.ControlFlow
  ( SelectionF, LoopF )
import {-# SOURCE #-} FIR.AST.Effs
  ( LetF, DefF, FunDefF, FunCallF, DefEntryPointF, StateF )
import {-# SOURCE #-} FIR.AST.Exts
  ( DebugPrintfF, RayF )
import {-# SOURCE #-} FIR.AST.Images
  ( ImgOpsF, ImgQueryF )
import {-# SOURCE #-} FIR.AST.Optics
  ( OpticF )
import {-# SOURCE #-} FIR.AST.Prim
  ( LamF, AppF, LitF, MkIDF, ValueF, UnsafeCoerceF
  , IxMonadF
  , PrimOpF, UndefinedF, GradedMappendF
  , ApplicativeF
  , MkVectorF, MatF, StructF, ArrayF, ArrayLengthF, HListF
  )

------------------------------------------------------------

type AllOpsF
  = '[ LamF, AppF, LitF, MkIDF, ValueF, UnsafeCoerceF
     , IxMonadF
     , PrimOpF, UndefinedF, GradedMappendF
     , ApplicativeF
     , MkVectorF, MatF, StructF, ArrayF, ArrayLengthF, HListF
     , SelectionF, LoopF
     , LetF, DefF, FunDefF, FunCallF, DefEntryPointF, StateF
     , DebugPrintfF, RayF
     , ImgOpsF, ImgQueryF
     , OpticF
     ]

type AST    = EGADT AllOpsF
type Code a = AST (Val a)

class Syntactic a where
  type Internal a :: AugType
  toAST :: a -> AST (Internal a)
  fromAST :: AST (Internal a) -> a

type InternalType a = UnderlyingType (Internal a)
