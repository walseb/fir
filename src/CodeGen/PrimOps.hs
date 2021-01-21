{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module: CodeGen.PrimOps

Code generation for primitive operations (e.g. arithmetic operations).
-}

module CodeGen.PrimOps
  ( primOp, operationInstruction )
  where

-- base
import Control.Monad
  ( when )
import Data.Proxy
  ( Proxy )

-- lens
import Control.Lens
  ( view )

-- mtl
import Control.Monad.Except
  ( throwError )

-- text-short
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Application
  ( Application(Applied)
  , traverseASTs
  )
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
import CodeGen.Debug
  ( whenAsserting )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( toArgs
  , ID(ID)
  , Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad, MonadFresh(fresh) )
import CodeGen.State
  ( _backend, _spirvVersion, requireCapabilities )
import FIR.AST
  ( AST, PrimOpF(..) )
import FIR.AST.Type
  ( Nullary )
import FIR.Prim.Op
  ( PrimOp(opName) )
import qualified SPIRV.PrimOp       as SPIRV
import qualified SPIRV.PrimTy       as SPIRV
import qualified SPIRV.Requirements as SPIRV
import qualified SPIRV.Operation    as SPIRV
  ( Operation )
import qualified SPIRV.Version      as SPIRV

----------------------------------------------------------------------------
-- primops


instance CodeGen AST => CodeGen (PrimOpF AST) where
  codeGenArgs ( Applied ( PrimOpF ( _ :: Proxy a ) ( _ :: Proxy op ) ) as ) =
    primOp ( opName @_ @_ @op @a ) =<< traverseASTs @Nullary codeGen as


primOp :: SPIRV.PrimOp -> [ (ID, SPIRV.PrimTy) ] -> CGMonad (ID, SPIRV.PrimTy)
primOp prim as = do
  bk <- view _backend
  let
    op :: SPIRV.Operation
    retTy :: SPIRV.PrimTy
    (op, retTy) = SPIRV.opAndReturnType bk prim
    minVer :: SPIRV.Version
    minVer = SPIRV.primOpMinVersion bk prim
  requireCapabilities $ SPIRV.primOpCapabilities bk prim
  ver <- view _spirvVersion
  whenAsserting . when ( ver < minVer ) . throwError $
    "primOp: operation " <> ShortText.pack ( show prim ) <> "\n\
    \requires SPIR-V version " <>  ShortText.pack ( show minVer ) <> ",\n\
    \but current version is  " <>  ShortText.pack ( show ver    ) <> "."
  operationInstruction op retTy ( map fst as )

operationInstruction :: SPIRV.Operation -> SPIRV.PrimTy -> [ ID ] -> CGMonad (ID, SPIRV.PrimTy)
operationInstruction op retTy as = case retTy of
  SPIRV.Unit -> do
    instruction
      Instruction
        { operation = op
        , resTy = Nothing
        , resID = Nothing
        , args = toArgs as
        }
    pure (ID 0, retTy) -- ID should not be used
  _ -> do
    resTyID <- typeID retTy
    v <- fresh
    instruction
      Instruction
        { operation = op
        , resTy = Just resTyID
        , resID = Just v
        , args = toArgs as
        }
    pure (v, retTy)
