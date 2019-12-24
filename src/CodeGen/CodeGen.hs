{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: CodeGen.CodeGen

Main code generation function: dispatches code generation on the constructors of the AST.
-}

module CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen, runCodeGen )
  where

-- base
import Prelude
  hiding ( Monad(..) )
import Data.Kind
  ( Type )
import Data.Proxy
  ( Proxy )
import Data.Type.Equality
  ( (:~:)(Refl) )
import qualified GHC.Stack
  ( callStack )
import Unsafe.Coerce
  ( unsafeCoerce )

-- bytestring
import Data.ByteString.Lazy
  ( ByteString )

-- haskus-utils-variant
import Haskus.Utils.VariantF
  ( VariantF, ApplyAll, BottomUp(toBottomUp) )
import Haskus.Utils.EGADT
  ( EGADT(..), HVariantF(..) )

-- mtl
import Control.Monad.Except
  ( throwError )

-- lens
import Control.Lens
  ( use, assign )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import CodeGen.Application
  ( Application(Applied)
  , pattern Nullary
  , ASTs(NilAST, ConsAST)
  )
import CodeGen.Applicative
  ( ) -- CodeGen instances for Ap, Pure
import CodeGen.Binary
  ( putModule )
import CodeGen.CFG
  ( ) -- CodeGen instances for If, IfM, Switch, SwitchM, While, Locally, Embed
import CodeGen.Composite
  ( ) -- CodeGen instances for MkVector, Mat, UnMat, Struct, Array, GradedMappend
import CodeGen.Debug
  ( whenDebugging, putSrcInfo )
import CodeGen.Functions
  ( ) -- CodeGen instances for FunDef, FunCall, DefEntryPoint
import CodeGen.IDs
  ( constID, undefID )
import CodeGen.Images
  ( )  -- CodeGen instances for image operands
import CodeGen.Instruction
  ( ID )
import CodeGen.Pointers
  ( newVariable, store )
import CodeGen.PrimOps
  ( ) -- CodeGen instance for PrimOp
import CodeGen.Monad
  ( CGMonad, runCGMonad
  , runExceptTPutM
  )
import CodeGen.Optics
  ( ) -- CodeGen for View, Set, Use, Assign
import CodeGen.State
  ( CGState, CGContext
  , _functionContext
  , _localBinding
  , initialState
  )
import Data.Type.Known
  ( knownValue )
import FIR.AST
  ( AST
  , pattern (:$), pattern Lam, pattern MkID
  , AppF(..), LamF(..), LitF(..), MkIDF(..), ValueF(..)
  , ReturnF(..), BindF(..)
  , DefF(..), UnsafeCoerceF(..)
  , NilHListF(..), ConsHListF(..)
  , UndefinedF(..)
  )
import FIR.AST.Type
  ( AugType(Val, (:-->)), FunArgs, Nullary )
import FIR.Binding
  ( Permission(Write) )
import FIR.Prim.Singletons
  ( primTy )
import FIR.ProgramState
  ( FunctionContext(TopLevel) )
import FIR.Syntax.AST
  ( )
import qualified SPIRV.PrimTy  as SPIRV
import qualified SPIRV.Storage as Storage

----------------------------------------------------------------------------
-- run the code generator with given context to create a SPIR-V module

runCodeGen :: Nullary a => CGContext -> AST a -> Either ShortText (ByteString, CGState)
runCodeGen context ast
  = case runCGMonad context (initialState context) (codeGen ast) of

      Right (_, cgState, body)
        -> case runExceptTPutM $ putModule context cgState of
              Right ((), decs) -> Right ( decs <> body, cgState )
              Left err         -> Left err

      Left err -> Left err

----------------------------------------------------------------------------
-- typeclass to dispatch code generation on AST constructors

class CodeGen (ast :: AugType -> Type) where
  codeGenArgs :: Nullary r => Application ast f r -> CGMonad (ID, SPIRV.PrimTy)

codeGen :: ( CodeGen ast, Nullary v ) => ast v -> CGMonad (ID, SPIRV.PrimTy)
codeGen v = codeGenArgs (Nullary v)

instance BottomUp CodeGen vs => CodeGen (VariantF vs) where
  codeGenArgs (Applied f args) =
    toBottomUp @CodeGen ( \x -> codeGenArgs $ Applied x args ) f

deriving via VariantF ( ApplyAll (EGADT vs :: AugType -> Type) vs )
  instance BottomUp CodeGen ( ApplyAll (EGADT vs) vs )
        => CodeGen (EGADT vs)

----------------------------------------------------------------------------
-- some instances

instance CodeGen AST => CodeGen (LamF AST) where
  codeGenArgs (Applied (LamF f) (a `ConsAST` as)) = do
    cg <- codeGen a
    codeGenArgs ( Applied ( f (MkID cg) ) as )

instance CodeGen (LitF AST) where
  codeGenArgs (Applied (LitF (a :: ty)) NilAST) = ( , primTy @ty ) <$> constID a

instance CodeGen AST => CodeGen (MkIDF AST) where
  codeGenArgs (Applied (MkIDF idTy) NilAST) = pure idTy

-- TODO: "Value" should not be a constructor of the AST
-- we should internally change the AST to add the Value constructor when doing evaluation
instance CodeGen (ValueF AST) where
  codeGenArgs _ = throwError "codeGen: unsupported embedded value"

instance CodeGen AST => CodeGen (AppF AST) where
  codeGenArgs (Applied (AppF (Lam f) a) as) = codeGenArgs (Applied (f a) as)
  codeGenArgs (Applied (AppF f a) as) = codeGenArgs (Applied f (a `ConsAST` as))

instance CodeGen AST => CodeGen (ReturnF AST) where
  codeGenArgs (Applied ReturnF (a `ConsAST` NilAST)) = codeGen a

instance CodeGen AST => CodeGen (BindF AST) where
  codeGenArgs (Applied BindF (a `ConsAST` f `ConsAST` as))
    | ( _ :: AST ( Val a :--> q j ) ) <- f
    , Refl <- ( unsafeCoerce Refl :: FunArgs (q j) :~: '[] )
    = do
      cg <- codeGen a
      codeGenArgs ( Applied ( f :$ MkID cg ) as )

instance CodeGen AST => CodeGen (UnsafeCoerceF AST) where
  codeGenArgs ( Applied UnsafeCoerceF (a `ConsAST` NilAST) ) = codeGenArgs ( Nullary a )

instance CodeGen AST => CodeGen (UndefinedF AST) where
  codeGenArgs ( Applied undef@UndefinedF NilAST ) = case undef of
    ( _ :: UndefinedF AST ( Val a ) ) ->
      let undefTy = primTy @a
      in ( , undefTy ) <$> undefID undefTy

instance CodeGen AST => CodeGen (NilHListF AST) where
  codeGenArgs _ = throwError "codeGen: internal error, cannot generate code for 'NilHList'"

instance CodeGen AST => CodeGen (ConsHListF AST) where
  codeGenArgs _ = throwError "codeGen: internal error, cannot generate code for 'ConsHList'"

instance CodeGen AST => CodeGen (DefF AST) where
  codeGenArgs (Applied (DefF (_ :: Proxy name) (_ :: Proxy ps)) (a `ConsAST` NilAST)) = do
    let name     = knownValue @name
        writable = Write `elem` (knownValue @ps)
    whenDebugging ( putSrcInfo GHC.Stack.callStack )
    cgRes@(a_ID, a_ty) <- codeGen a

    -- check if we should make this into a pointer or not
    let
      makeMutable =
        case a_ty of
          SPIRV.Array        {} -> writable
          SPIRV.RuntimeArray {} -> writable -- should never happen, user cannot define runtime arrays
          SPIRV.Struct       {} -> writable
          _                     -> False

    defRes <-
      if makeMutable
      then do
        ctx <- use _functionContext
        let storage =
              case ctx of
                TopLevel -> Storage.Private
                _        -> Storage.Function
        let ptrTy = SPIRV.PointerTy storage a_ty
        ptrID <- newVariable ptrTy
        store (name, a_ID) ptrID ptrTy
        pure (ptrID, SPIRV.pointerTy ptrTy)
      else pure cgRes

    assign ( _localBinding name ) ( Just defRes )
    -- addName ( fst defRes ) name -- (not necessarily helpful to assign names as we're using SSA)
    pure cgRes
