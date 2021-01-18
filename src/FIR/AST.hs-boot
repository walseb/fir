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
  ( IfF, IfMF, SwitchF, SwitchMF, WhileF )
import {-# SOURCE #-} FIR.AST.Effs
  ( LetF, DefF, FunDefF, FunCallF, DefEntryPointF, LocallyF, EmbedF )
import {-# SOURCE #-} FIR.AST.Exts
  ( DebugPrintfF, TraceRayF, ExecuteCallableF, RayQueryF )
import {-# SOURCE #-} FIR.AST.Images
  ( ImgOpsF, ImgQueryF )
import {-# SOURCE #-} FIR.AST.Optics
  ( UseF, AssignF, ViewF, SetF )
import {-# SOURCE #-} FIR.AST.Prim
  ( LamF, AppF, LitF, MkIDF, ValueF, UnsafeCoerceF
  , ReturnF, BindF
  , PrimOpF, UndefinedF, GradedMappendF
  , PureF, ApF
  , MkVectorF, MatF, UnMatF, StructF, ArrayF, ArrayLengthF, NilHListF, ConsHListF
  )

------------------------------------------------------------

type AllOpsF
  = '[ LamF, AppF, LitF, MkIDF, ValueF, UnsafeCoerceF
     , ReturnF, BindF
     , PrimOpF, UndefinedF, GradedMappendF
     , PureF, ApF
     , MkVectorF, MatF, UnMatF, StructF, ArrayF, ArrayLengthF, NilHListF, ConsHListF
     , IfF, IfMF, SwitchF, SwitchMF, WhileF
     , LetF, DefF, FunDefF, FunCallF, DefEntryPointF, LocallyF, EmbedF
     , DebugPrintfF, TraceRayF, ExecuteCallableF, RayQueryF
     , ImgOpsF, ImgQueryF
     , UseF, AssignF, ViewF, SetF
     ]

type AST    = EGADT AllOpsF
type Code a = AST (Val a)

class Syntactic a where
  type Internal a :: AugType
  toAST :: a -> AST (Internal a)
  fromAST :: AST (Internal a) -> a

type InternalType a = UnderlyingType (Internal a)
