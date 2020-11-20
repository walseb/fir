{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module: CodeGen.PrimOps

Code generation for primitive operations (e.g. arithmetic operations).
-}

module CodeGen.PrimOps (primOp) where

-- base
import Data.Proxy
  ( Proxy )

-- lens
import Control.Lens
  ( view )

-- fir
import CodeGen.Application
  ( Application(Applied)
  , traverseASTs
  )
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
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
  ( _backend, requireCapabilities )
import FIR.AST
  ( AST, PrimOpF(..) )
import FIR.AST.Type
  ( Nullary )
import FIR.Prim.Op
  ( PrimOp(opName) )
import qualified SPIRV.PrimOp       as SPIRV
import qualified SPIRV.PrimTy       as SPIRV
import qualified SPIRV.Requirements as SPIRV

----------------------------------------------------------------------------
-- primops


instance CodeGen AST => CodeGen (PrimOpF AST) where
  codeGenArgs ( Applied ( PrimOpF ( _ :: Proxy a ) ( _ :: Proxy op ) ) as ) =
    primOp ( opName @_ @_ @op @a ) =<< traverseASTs @Nullary codeGen as


primOp :: SPIRV.PrimOp -> [ (ID, SPIRV.PrimTy) ] -> CGMonad (ID, SPIRV.PrimTy)
primOp prim as = do

  bk <- view _backend
  let (op, retTy) = SPIRV.opAndReturnType bk prim

  requireCapabilities $ SPIRV.primOpCapabilities bk prim

  case retTy of
    SPIRV.Unit -> do
      instruction
        Instruction
          { operation = op
          , resTy = Nothing
          , resID = Nothing
          , args = toArgs (map fst as)
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
          , args = toArgs (map fst as)
          }
      pure (v, retTy)
